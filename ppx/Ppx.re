open Ppxlib;
open Ast_helper;

module ExternalModule = {
  type t = string;
  let compare = Pervasives.compare;
};

module ExternalModuleSet = {
  include Set.Make(ExternalModule);

  let concat = (x1: t, x2: t): t => {
    fold((m, acc) => acc |> add(m), x1, x2);
  };
};

module Result = {
  let map = (fn: 'x => 'y, x: result('x, 'e)): result('y, 'e) =>
    switch (x) {
    | Ok(x) => Ok(x |> fn)
    | Error(e) => Error(e)
    };

  let flatMap =
      (fn: 'x => result('y, 'e), x: result('x, 'e)): result('y, 'e) =>
    switch (x) {
    | Ok(x) => x |> fn
    | Error(e) => Error(e)
    };
};

module ParsedData = {
  type t = {
    css: string,
    modules: ExternalModuleSet.t,
  };

  let empty = {css: "", modules: ExternalModuleSet.empty};
};

module Error = {
  type t = {
    reason: [
      | `UnexpectedInterpolation
      | `UnexpectedFunction(
          [
            | `LabellledArg
            | `OptionalArg
            | `UnexpectedPipe
            | `PlaceholderArg
          ],
        )
    ],
    loc: location,
  };
};

let js_interpolation = (x: string) => "${" ++ x ++ "}";

let js_infix = (~l: string, ~op: string, ~r: string) =>
  l ++ " " ++ op ++ " " ++ r;

let join_loc = (loc1, loc2) => {
  let cmp = Location.compare(loc1, loc2);
  let (l_loc, r_loc) =
    if (cmp >= 0) {
      (loc1, loc2);
    } else {
      (loc2, loc1);
    };
  {
    loc_start: l_loc.loc_start,
    loc_end: r_loc.loc_end,
    loc_ghost: l_loc.loc_ghost,
  };
};

let parse_lid = (lid: longident) => {
  let parts = lid |> Longident.flatten_exn;
  switch (parts) {
  | [] => ParsedData.empty
  | [x] => {ParsedData.css: x, modules: ExternalModuleSet.empty}
  | [hd, ...tl] => {
      ParsedData.css: tl |> List.fold_left((acc, x) => acc ++ "." ++ x, hd),
      modules: hd |> ExternalModuleSet.singleton,
    }
  };
};

let rec parse_function = (~args: list((arg_label, expression)), ~loc, lid) => {
  let fn_lid = lid.txt |> parse_lid;
  switch (fn_lid.css) {
  | "|." => fn_lid |> parse_pipe_function(~data=`first, ~args, ~loc)
  | "|>" => fn_lid |> parse_pipe_function(~data=`last, ~args, ~loc)
  | _ => fn_lid |> parse_general_function(~args)
  };
}
and parse_pipe_function =
    (
      ~data: [ | `first | `last],
      ~args: list((arg_label, expression)),
      ~loc: location,
      lid: ParsedData.t,
    ) => {
  switch (args) {
  | [
      (Nolabel, _) as l_arg,
      (Nolabel, {pexp_desc: Pexp_ident({txt: r_lid})}),
    ] =>
    r_lid |> parse_lid |> parse_general_function(~args=[l_arg])

  | [
      (Nolabel, _) as l_arg,
      (
        Nolabel,
        {
          pexp_desc:
            Pexp_apply({pexp_desc: Pexp_ident({txt: r_lid})}, r_args),
        },
      ),
    ] =>
    r_lid
    |> parse_lid
    |> parse_general_function(
         ~args=
           switch (data) {
           | `first => [l_arg, ...r_args]
           | `last => List.append(r_args, [l_arg])
           },
       )

  | [
      _,
      (
        Nolabel,
        {pexp_desc: Pexp_fun(_, _, {ppat_desc: Ppat_var(_), ppat_loc}, _)},
      ),
    ] =>
    Error({
      Error.reason: `UnexpectedFunction(`PlaceholderArg),
      loc: ppat_loc,
    })

  | _ => Error({Error.reason: `UnexpectedFunction(`UnexpectedPipe), loc})
  };
}
and parse_general_function =
    (~args: list((arg_label, expression)), lid: ParsedData.t) => {
  switch (args) {
  | [(Nolabel, {pexp_desc: Pexp_construct({txt: Lident("()")}, _)})] =>
    Ok({ParsedData.css: lid.css ++ "()", modules: lid.modules})
  | _ as args =>
    args
    |> List.fold_left(
         (res, arg) =>
           res
           |> Result.flatMap((data: ParsedData.t) =>
                switch (arg) {
                | (Nolabel, expr) =>
                  expr
                  |> js_arg_from_expr
                  |> Result.map((arg: ParsedData.t) =>
                       {
                         ParsedData.css:
                           switch (data.css) {
                           | "" => arg.css
                           | _ as css => css ++ ", " ++ arg.css
                           },
                         modules:
                           data.modules
                           |> ExternalModuleSet.concat(arg.modules),
                       }
                     )
                | (Labelled(_), {pexp_loc}) =>
                  Error({
                    Error.reason: `UnexpectedFunction(`LabellledArg),
                    loc: pexp_loc,
                  })
                | (Optional(_), {pexp_loc}) =>
                  Error({
                    Error.reason: `UnexpectedFunction(`OptionalArg),
                    loc: pexp_loc,
                  })
                }
              ),
         Ok(ParsedData.empty),
       )
    |> Result.map((args: ParsedData.t) =>
         {
           ParsedData.css: lid.css ++ "(" ++ args.css ++ ")",
           modules: lid.modules |> ExternalModuleSet.concat(args.modules),
         }
       )
  };
}
and js_arg_from_expr = (expr: expression) =>
  switch (expr) {
  | {pexp_desc: Pexp_ident({txt: lid})} => Ok(lid |> parse_lid)
  | {pexp_desc: Pexp_constant(Pconst_integer(x, _) | Pconst_float(x, _))} =>
    Ok({ParsedData.css: x, modules: ExternalModuleSet.empty})
  | {pexp_desc: Pexp_constant(Pconst_string(x, _, _))} =>
    Ok({ParsedData.css: "\"" ++ x ++ "\"", modules: ExternalModuleSet.empty})
  | {pexp_desc: Pexp_apply({pexp_desc: Pexp_ident(lid)}, args), pexp_loc} =>
    lid |> parse_function(~args, ~loc=pexp_loc)
  | {pexp_loc} =>
    Error({Error.reason: `UnexpectedInterpolation, loc: pexp_loc})
  };

let rec concat_js_expression =
        (
          ~op: string,
          ~loc: location,
          ~res: result(ParsedData.t, Error.t),
          operands: list((arg_label, expression)),
        )
        : result(ParsedData.t, Error.t) => {
  res
  |> Result.flatMap((data: ParsedData.t) =>
       switch (operands) {
       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {
                     pexp_desc:
                       Pexp_ident({
                         txt:
                           Lident(
                             "+" as l_op | "-" as l_op | "*" as l_op |
                             "/" as l_op,
                           ),
                       }),
                   },
                   l_operands,
                 ),
               pexp_loc: l_apply_loc,
             },
           ),
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {
                     pexp_desc:
                       Pexp_ident({
                         txt:
                           Lident(
                             "+" as r_op | "-" as r_op | "*" as r_op |
                             "/" as r_op,
                           ),
                       }),
                   },
                   r_operands,
                 ),
               pexp_loc: r_apply_loc,
             },
           ),
         ] =>
         switch (
           l_operands
           |> concat_js_expression(
                ~op=l_op,
                ~loc=l_apply_loc,
                ~res=Ok(ParsedData.empty),
              ),
           r_operands
           |> concat_js_expression(
                ~op=r_op,
                ~loc=r_apply_loc,
                ~res=Ok(ParsedData.empty),
              ),
         ) {
         | (Ok(l), Ok(r)) =>
           Ok({
             ParsedData.css: js_infix(~l=l.css, ~op, ~r=r.css) ++ data.css,
             modules:
               data.modules
               |> ExternalModuleSet.concat(l.modules)
               |> ExternalModuleSet.concat(r.modules),
           })
         | (Error(error), _)
         | (_, Error(error)) => Error(error)
         }
       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {
                     pexp_desc:
                       Pexp_ident({
                         txt:
                           Lident(
                             "+" as l_op | "-" as l_op | "*" as l_op |
                             "/" as l_op,
                           ),
                       }),
                   },
                   l_operands,
                 ),
               pexp_loc: l_apply_loc,
             },
           ),
           (Nolabel, r_expr),
         ] =>
         r_expr
         |> js_arg_from_expr
         |> Result.flatMap((r: ParsedData.t) =>
              l_operands
              |> concat_js_expression(
                   ~op=l_op,
                   ~loc=l_apply_loc,
                   ~res=Ok(ParsedData.empty),
                 )
              |> Result.flatMap((l: ParsedData.t) =>
                   Ok({
                     ParsedData.css:
                       js_infix(~l=l.css, ~op, ~r=r.css) ++ data.css,
                     modules:
                       data.modules
                       |> ExternalModuleSet.concat(l.modules)
                       |> ExternalModuleSet.concat(r.modules),
                   })
                 )
            )
       | [
           (Nolabel, l_expr),
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {
                     pexp_desc:
                       Pexp_ident({
                         txt:
                           Lident(
                             "+" as r_op | "-" as r_op | "*" as r_op |
                             "/" as r_op,
                           ),
                       }),
                   },
                   r_operands,
                 ),
               pexp_loc: r_apply_loc,
             },
           ),
         ] =>
         l_expr
         |> js_arg_from_expr
         |> Result.flatMap((l: ParsedData.t) =>
              r_operands
              |> concat_js_expression(
                   ~op=r_op,
                   ~loc=r_apply_loc,
                   ~res=Ok(ParsedData.empty),
                 )
              |> Result.flatMap((r: ParsedData.t) =>
                   Ok({
                     ParsedData.css:
                       js_infix(~l=l.css, ~op, ~r=r.css) ++ data.css,
                     modules:
                       data.modules
                       |> ExternalModuleSet.concat(l.modules)
                       |> ExternalModuleSet.concat(r.modules),
                   })
                 )
            )
       | [(Nolabel, l_expr), (Nolabel, r_expr)] =>
         switch (l_expr |> js_arg_from_expr, r_expr |> js_arg_from_expr) {
         | (Ok(l), Ok(r)) =>
           Ok({
             ParsedData.css: js_infix(~l=l.css, ~op, ~r=r.css) ++ data.css,
             modules:
               data.modules
               |> ExternalModuleSet.concat(l.modules)
               |> ExternalModuleSet.concat(r.modules),
           })
         | (Error(error), _)
         | (_, Error(error)) => Error(error)
         }
       | _ as operands =>
         let hd = List.nth_opt(operands, 0);
         let tl = List.nth_opt(operands |> List.rev, 0);
         let loc =
           switch (hd, tl) {
           | (Some((_, {pexp_loc: loc1})), Some((_, {pexp_loc: loc2}))) =>
             join_loc(loc1, loc2)
           | (Some((_, {pexp_loc: loc})), None)
           | (None, Some((_, {pexp_loc: loc}))) => loc
           | (None, None) => loc
           };
         Error({Error.reason: `UnexpectedInterpolation, loc});
       }
     );
};

let rec concat_css_block =
        (
          ~loc: location,
          ~res: result(ParsedData.t, Error.t),
          operands: list((arg_label, expression)),
        ) => {
  res
  |> Result.flatMap((data: ParsedData.t) =>
       switch (operands) {
       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {pexp_desc: Pexp_ident({txt: Lident("^")})},
                   l_operands,
                 ),
               pexp_loc: l_apply_loc,
             },
           ),
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_constant(Pconst_string(r_css, _r_loc, Some("css"))),
             },
           ),
         ] =>
         l_operands
         |> concat_css_block(
              ~loc=l_apply_loc,
              ~res=
                Ok({
                  ParsedData.css: r_css ++ data.css,
                  modules: data.modules,
                }),
            )

       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {pexp_desc: Pexp_ident({txt: Lident("^")})},
                   l_operands,
                 ),
               pexp_loc: l_apply_loc,
             },
           ),
           (Nolabel, {pexp_desc: Pexp_ident({txt: r_lid})}),
         ] =>
         let r_lid = r_lid |> parse_lid;
         l_operands
         |> concat_css_block(
              ~loc=l_apply_loc,
              ~res=
                Ok({
                  ParsedData.css: js_interpolation(r_lid.css) ++ data.css,
                  modules:
                    data.modules |> ExternalModuleSet.concat(r_lid.modules),
                }),
            );

       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {pexp_desc: Pexp_ident({txt: Lident("^")})},
                   l_operands,
                 ),
               pexp_loc: l_apply_loc,
             },
           ),
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_constant(
                   Pconst_integer(r_css, _) | Pconst_float(r_css, _),
                 ),
             },
           ),
         ] =>
         l_operands
         |> concat_css_block(
              ~loc=l_apply_loc,
              ~res=
                Ok({
                  ParsedData.css: js_interpolation(r_css) ++ data.css,
                  modules: data.modules,
                }),
            )

       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_constant(Pconst_string(l_css, _l_loc, Some("css"))),
             },
           ),
           (Nolabel, {pexp_desc: Pexp_ident({txt: r_lid})}),
         ] =>
         let r_lid = r_lid |> parse_lid;
         Ok({
           ParsedData.css: l_css ++ js_interpolation(r_lid.css) ++ data.css,
           modules: data.modules |> ExternalModuleSet.concat(r_lid.modules),
         });

       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_constant(Pconst_string(l_css, _l_loc, Some("css"))),
             },
           ),
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_constant(
                   Pconst_integer(r_css, _) | Pconst_float(r_css, _),
                 ),
             },
           ),
         ] =>
         Ok({
           ParsedData.css: l_css ++ js_interpolation(r_css) ++ data.css,
           modules: data.modules,
         })

       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_constant(Pconst_string(l_css, _l_loc, Some("css"))),
             },
           ),
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_constant(Pconst_string(r_css, _r_loc, Some("css"))),
             },
           ),
         ] =>
         Ok({
           ParsedData.css: l_css ++ r_css ++ data.css,
           modules: data.modules,
         })

       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {pexp_desc: Pexp_ident({txt: Lident("^")})},
                   l_operands,
                 ),
               pexp_loc: l_apply_loc,
             },
           ),
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {
                     pexp_desc:
                       Pexp_ident({
                         txt:
                           Lident(
                             "+" as r_op | "-" as r_op | "*" as r_op |
                             "/" as r_op,
                           ),
                       }),
                     pexp_loc: r_exp_loc,
                   },
                   r_operands,
                 ),
             },
           ),
         ] =>
         r_operands
         |> concat_js_expression(
              ~op=r_op,
              ~loc=r_exp_loc,
              ~res=Ok(ParsedData.empty),
            )
         |> Result.flatMap((r: ParsedData.t) =>
              l_operands
              |> concat_css_block(
                   ~loc=l_apply_loc,
                   ~res=Ok(ParsedData.empty),
                 )
              |> Result.flatMap((l: ParsedData.t) =>
                   Ok({
                     ParsedData.css:
                       l.css ++ js_interpolation(r.css) ++ data.css,
                     modules:
                       data.modules
                       |> ExternalModuleSet.concat(l.modules)
                       |> ExternalModuleSet.concat(r.modules),
                   })
                 )
            )

       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_constant(Pconst_string(l_css, _l_loc, Some("css"))),
             },
           ),
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {
                     pexp_desc:
                       Pexp_ident({
                         txt:
                           Lident(
                             "+" as r_op | "-" as r_op | "*" as r_op |
                             "/" as r_op,
                           ),
                       }),
                     pexp_loc: r_exp_loc,
                   },
                   r_operands,
                 ),
             },
           ),
         ] =>
         r_operands
         |> concat_js_expression(
              ~op=r_op,
              ~loc=r_exp_loc,
              ~res=Ok(ParsedData.empty),
            )
         |> Result.flatMap((r: ParsedData.t) =>
              Ok({
                ParsedData.css: l_css ++ js_interpolation(r.css) ++ data.css,
                modules: data.modules |> ExternalModuleSet.concat(r.modules),
              })
            )

       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_constant(Pconst_string(l_css, _l_loc, Some("css"))),
             },
           ),
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply({pexp_desc: Pexp_ident(r_lid)}, r_operands),
               pexp_loc: r_exp_loc,
             },
           ),
         ] =>
         r_lid
         |> parse_function(~args=r_operands, ~loc=r_exp_loc)
         |> Result.flatMap((r: ParsedData.t) =>
              Ok({
                ParsedData.css: l_css ++ js_interpolation(r.css) ++ data.css,
                modules: data.modules |> ExternalModuleSet.concat(r.modules),
              })
            )

       | [
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply(
                   {pexp_desc: Pexp_ident({txt: Lident("^")})},
                   l_operands,
                 ),
               pexp_loc: l_apply_loc,
             },
           ),
           (
             Nolabel,
             {
               pexp_desc:
                 Pexp_apply({pexp_desc: Pexp_ident(r_lid)}, r_operands),
               pexp_loc: r_exp_loc,
             },
           ),
         ] =>
         r_lid
         |> parse_function(~args=r_operands, ~loc=r_exp_loc)
         |> Result.flatMap((r: ParsedData.t) =>
              l_operands
              |> concat_css_block(
                   ~loc=l_apply_loc,
                   ~res=Ok(ParsedData.empty),
                 )
              |> Result.flatMap((l: ParsedData.t) =>
                   Ok({
                     ParsedData.css:
                       l.css ++ js_interpolation(r.css) ++ data.css,
                     modules:
                       data.modules
                       |> ExternalModuleSet.concat(l.modules)
                       |> ExternalModuleSet.concat(r.modules),
                   })
                 )
            )

       | _ as operands =>
         let hd = List.nth_opt(operands, 0);
         let tl = List.nth_opt(operands |> List.rev, 0);
         let loc =
           switch (hd, tl) {
           | (Some((_, {pexp_loc: loc1})), Some((_, {pexp_loc: loc2}))) =>
             join_loc(loc1, loc2)
           | (Some((_, {pexp_loc: loc})), None)
           | (None, Some((_, {pexp_loc: loc}))) => loc
           | (None, None) => loc
           };
         Error({Error.reason: `UnexpectedInterpolation, loc});
       }
     );
};

let ext =
  Extension.declare(
    "css",
    Extension.Context.module_expr,
    Ast_pattern.__,
    (~loc, ~path as _, expr) => {
    switch (expr) {
    | PStr(str) =>
      // This is required import so babel plugin could pickup this module
      let import = [%stri
        %raw
        {|import { css } from "@linaria/core"|}
      ];

      let (str, modules) =
        str
        |> List.rev
        |> List.fold_left(
             ((str, modules), item) =>
               switch (item) {
               | {
                   pstr_desc:
                     Pstr_value(
                       Nonrecursive,
                       [
                         {
                           pvb_pat: {
                             ppat_desc: Ppat_var(var),
                             ppat_loc,
                             ppat_loc_stack,
                             ppat_attributes,
                           },
                           pvb_expr: {
                             pexp_desc:
                               Pexp_constant(
                                 Pconst_string(css, _loc, Some("css")),
                               ),
                             pexp_loc,
                             pexp_loc_stack,
                             pexp_attributes,
                           },
                           pvb_attributes,
                           pvb_loc,
                         },
                       ],
                     ),
                   pstr_loc,
                 } => (
                   [
                     {
                       pstr_desc:
                         Pstr_value(
                           Nonrecursive,
                           [
                             {
                               pvb_pat: {
                                 ppat_desc: Ppat_var(var),
                                 ppat_loc,
                                 ppat_loc_stack,
                                 ppat_attributes,
                               },
                               pvb_expr: {
                                 let loc = pexp_loc;
                                 let typ = [%type: string];
                                 let exp = [%expr
                                   [%raw
                                     [%e
                                       Exp.constant(
                                         Const.string("css`" ++ css ++ "`"),
                                       )
                                     ]
                                   ]
                                 ];
                                 Ast_helper.Exp.constraint_(~loc, exp, typ);
                               },
                               pvb_attributes,
                               pvb_loc,
                             },
                           ],
                         ),
                       pstr_loc,
                     },
                     ...str,
                   ],
                   modules,
                 )
               | {
                   pstr_desc:
                     Pstr_value(
                       Nonrecursive,
                       [
                         {
                           pvb_pat: {
                             ppat_desc: Ppat_var(var),
                             ppat_loc,
                             ppat_loc_stack,
                             ppat_attributes,
                           },
                           pvb_expr: {
                             pexp_desc:
                               Pexp_apply(
                                 {pexp_desc: Pexp_ident({txt: Lident("^")})},
                                 [
                                   (Nolabel, _),
                                   (
                                     Nolabel,
                                     {
                                       pexp_desc:
                                         Pexp_constant(
                                           Pconst_string(_, _, Some("css")),
                                         ),
                                     },
                                   ),
                                 ] as css,
                               ),
                             pexp_loc,
                             pexp_loc_stack,
                             pexp_attributes,
                           },
                           pvb_attributes,
                           pvb_loc,
                         },
                       ],
                     ),
                   pstr_loc,
                 } =>
                 switch (
                   css
                   |> concat_css_block(
                        ~loc=pexp_loc,
                        ~res=Ok(ParsedData.empty),
                      )
                 ) {
                 | Ok(data) => (
                     [
                       {
                         pstr_desc:
                           Pstr_value(
                             Nonrecursive,
                             [
                               {
                                 pvb_pat: {
                                   ppat_desc: Ppat_var(var),
                                   ppat_loc,
                                   ppat_loc_stack,
                                   ppat_attributes,
                                 },
                                 pvb_expr: {
                                   let loc = pexp_loc;
                                   let typ = [%type: string];
                                   let exp = [%expr
                                     [%raw
                                       [%e
                                         Exp.constant(
                                           Const.string(
                                             "css`" ++ data.css ++ "`",
                                           ),
                                         )
                                       ]
                                     ]
                                   ];
                                   Ast_helper.Exp.constraint_(~loc, exp, typ);
                                 },
                                 pvb_attributes,
                                 pvb_loc,
                               },
                             ],
                           ),
                         pstr_loc,
                       },
                       ...str,
                     ],
                     modules |> ExternalModuleSet.concat(data.modules),
                   )
                 | Error({reason, loc}) =>
                   switch (reason) {
                   | `UnexpectedInterpolation =>
                     Location.raise_errorf(~loc, "Unexpected interpolation")
                   | `UnexpectedFunction(`LabellledArg) =>
                     Location.raise_errorf(
                       ~loc,
                       "Functions with labelled arguments are not supported",
                     )
                   | `UnexpectedFunction(`OptionalArg) =>
                     Location.raise_errorf(
                       ~loc,
                       "Functions with optional arguments are not supported",
                     )
                   | `UnexpectedFunction(`PlaceholderArg) =>
                     Location.raise_errorf(
                       ~loc,
                       "Pipe placeholders are not supported",
                     )
                   | `UnexpectedFunction(`UnexpectedPipe) =>
                     Location.raise_errorf(
                       ~loc,
                       "Function application with pipe is supported, but I can't parse this combination. Please, file an issue with your use-case.",
                     )
                   }
                 }
               | _ as item => ([item, ...str], modules)
               },
             ([], ExternalModuleSet.empty),
           );

      let includes =
        modules
        |> ExternalModuleSet.elements
        |> List.map(m => {
             Ast_helper.Str.include_(
               ~loc,
               Incl.mk(Mod.ident(~loc, {txt: Lident(m), loc})),
             )
           });

      Mod.mk(Pmod_structure(str |> List.append([import, ...includes])));

    | _ => Location.raise_errorf(~loc, "Must be a module")
    }
  });

"rescript-linaria"
|> Ppxlib.Driver.register_transformation(~extensions=[ext]);
