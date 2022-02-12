open Ppxlib;
open Ast_helper;

module ExternalModule = {
  type t = string;
  let compare = Stdlib.compare;
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

// A list of JS globals that ReScript compiler escapes
// Source: https://github.com/rescript-lang/rescript-compiler/blob/88645d034c663cd692e81ff6ffc15aadae0e165d/jscomp/keywords.list
// Unocommented items are the ones that might be used as names for style-related ReScript modules
let js_globals = [|
  "Object",
  // "Function",
  // "Array",
  // "Number",
  // "Infinity",
  // "NaN",
  // "Boolean",
  "String",
  "Symbol",
  "Date",
  // "Promise",
  // "RegExp",
  "Error",
  // "EvalError",
  // "RangeError",
  // "ReferenceError",
  // "SyntaxError",
  // "TypeError",
  // "URIError",
  // "JSON",
  // "Math",
  // "Intl",
  // "ArrayBuffer",
  // "Uint8Array",
  // "Int8Array",
  // "Uint16Array",
  // "Int16Array",
  // "Uint32Array",
  // "Int32Array",
  // "Float32Array",
  // "Float64Array",
  // "Uint8ClampedArray",
  // "BigUint64Array",
  // "BigInt64Array",
  "DataView",
  "Map",
  // "BigInt",
  // "Set",
  // "WeakMap",
  // "WeakSet",
  // "Proxy",
  // "Reflect",
  // "ByteLengthQueuingStrategy",
  // "CountQueuingStrategy",
  // "WritableStream",
  // "WebSocket",
  // "WebGLContextEvent",
  // "WaveShaperNode",
  // "TextEncoder",
  // "TextDecoder",
  // "SyncManager",
  // "SubtleCrypto",
  // "StorageEvent",
  "Storage",
  // "StereoPannerNode",
  // "SourceBufferList",
  // "SourceBuffer",
  // "ScriptProcessorNode",
  // "ScreenOrientation",
  // "RTCTrackEvent",
  // "RTCStatsReport",
  // "RTCSessionDescription",
  // "RTCRtpTransceiver",
  // "RTCRtpSender",
  // "RTCRtpReceiver",
  // "RTCRtpContributingSource",
  // "RTCPeerConnectionIceEvent",
  // "RTCPeerConnection",
  // "RTCIceCandidate",
  // "RTCDataChannelEvent",
  // "RTCDataChannel",
  // "RTCDTMFToneChangeEvent",
  // "RTCDTMFSender",
  // "RTCCertificate",
  // "Plugin",
  // "PluginArray",
  // "PhotoCapabilities",
  // "PeriodicWave",
  // "PannerNode",
  // "OverconstrainedError",
  // "OscillatorNode",
  // "OfflineAudioContext",
  // "OfflineAudioCompletionEvent",
  // "NetworkInformation",
  // "MimeType",
  // "MimeTypeArray",
  // "MediaStreamTrackEvent",
  // "MediaStreamTrack",
  // "MediaStreamEvent",
  // "MediaStream",
  // "MediaStreamAudioSourceNode",
  // "MediaStreamAudioDestinationNode",
  // "MediaSource",
  // "MediaSettingsRange",
  // "MediaRecorder",
  // "MediaEncryptedEvent",
  // "MediaElementAudioSourceNode",
  // "MediaDevices",
  // "MediaDeviceInfo",
  // "MediaCapabilities",
  // "MIDIPort",
  // "MIDIOutputMap",
  // "MIDIOutput",
  // "MIDIMessageEvent",
  // "MIDIInputMap",
  // "MIDIInput",
  // "MIDIConnectionEvent",
  // "MIDIAccess",
  // "InputDeviceInfo",
  // "ImageCapture",
  // "ImageBitmapRenderingContext",
  // "IIRFilterNode",
  // "IDBVersionChangeEvent",
  // "IDBTransaction",
  // "IDBRequest",
  // "IDBOpenDBRequest",
  // "IDBObjectStore",
  // "IDBKeyRange",
  // "IDBIndex",
  // "IDBFactory",
  // "IDBDatabase",
  // "IDBCursorWithValue",
  // "IDBCursor",
  // "GamepadEvent",
  // "Gamepad",
  // "GamepadButton",
  // "GainNode",
  // "EventSource",
  // "DynamicsCompressorNode",
  // "DeviceOrientationEvent",
  // "DeviceMotionEvent",
  // "DelayNode",
  // "DOMError",
  // "CryptoKey",
  // "Crypto",
  // "ConvolverNode",
  // "ConstantSourceNode",
  // "CloseEvent",
  // "ChannelSplitterNode",
  // "ChannelMergerNode",
  // "CanvasRenderingContext2D",
  // "CanvasCaptureMediaStreamTrack",
  // "BroadcastChannel",
  // "BlobEvent",
  // "BiquadFilterNode",
  // "BeforeInstallPromptEvent",
  // "BatteryManager",
  // "BaseAudioContext",
  // "AudioWorkletNode",
  // "AudioScheduledSourceNode",
  // "AudioProcessingEvent",
  // "AudioParamMap",
  // "AudioParam",
  // "AudioNode",
  // "AudioListener",
  // "AudioDestinationNode",
  // "AudioContext",
  // "AudioBufferSourceNode",
  // "AudioBuffer",
  // "AnalyserNode",
  // "XPathResult",
  // "XPathExpression",
  // "XPathEvaluator",
  // "XMLSerializer",
  // "XMLHttpRequestUpload",
  // "XMLHttpRequestEventTarget",
  // "XMLHttpRequest",
  // "XMLDocument",
  // "Worker",
  "Window",
  // "WheelEvent",
  // "VisualViewport",
  // "ValidityState",
  // "VTTCue",
  // "URLSearchParams",
  // "URL",
  // "UIEvent",
  // "TreeWalker",
  // "TransitionEvent",
  // "TransformStream",
  // "TrackEvent",
  // "TouchList",
  // "TouchEvent",
  "Touch",
  // "TimeRanges",
  // "TextTrackList",
  // "TextTrackCueList",
  // "TextTrackCue",
  // "TextTrack",
  // "TextMetrics",
  // "TextEvent",
  "Text",
  // "TaskAttributionTiming",
  // "StyleSheetList",
  "StyleSheet",
  // "StylePropertyMapReadOnly",
  // "StylePropertyMap",
  // "StaticRange",
  // "ShadowRoot",
  "Selection",
  // "SecurityPolicyViolationEvent",
  "Screen",
  // "SVGViewElement",
  // "SVGUseElement",
  // "SVGUnitTypes",
  // "SVGTransformList",
  // "SVGTransform",
  // "SVGTitleElement",
  // "SVGTextPositioningElement",
  // "SVGTextPathElement",
  // "SVGTextElement",
  // "SVGTextContentElement",
  // "SVGTSpanElement",
  // "SVGSymbolElement",
  // "SVGSwitchElement",
  // "SVGStyleElement",
  // "SVGStringList",
  // "SVGStopElement",
  // "SVGSetElement",
  // "SVGScriptElement",
  // "SVGSVGElement",
  // "SVGRectElement",
  // "SVGRect",
  // "SVGRadialGradientElement",
  // "SVGPreserveAspectRatio",
  // "SVGPolylineElement",
  // "SVGPolygonElement",
  // "SVGPointList",
  // "SVGPoint",
  // "SVGPatternElement",
  // "SVGPathElement",
  // "SVGNumberList",
  // "SVGNumber",
  // "SVGMetadataElement",
  // "SVGMatrix",
  // "SVGMaskElement",
  // "SVGMarkerElement",
  // "SVGMPathElement",
  // "SVGLinearGradientElement",
  // "SVGLineElement",
  // "SVGLengthList",
  // "SVGLength",
  // "SVGImageElement",
  // "SVGGraphicsElement",
  // "SVGGradientElement",
  // "SVGGeometryElement",
  // "SVGGElement",
  // "SVGForeignObjectElement",
  // "SVGFilterElement",
  // "SVGFETurbulenceElement",
  // "SVGFETileElement",
  // "SVGFESpotLightElement",
  // "SVGFESpecularLightingElement",
  // "SVGFEPointLightElement",
  // "SVGFEOffsetElement",
  // "SVGFEMorphologyElement",
  // "SVGFEMergeNodeElement",
  // "SVGFEMergeElement",
  // "SVGFEImageElement",
  // "SVGFEGaussianBlurElement",
  // "SVGFEFuncRElement",
  // "SVGFEFuncGElement",
  // "SVGFEFuncBElement",
  // "SVGFEFuncAElement",
  // "SVGFEFloodElement",
  // "SVGFEDropShadowElement",
  // "SVGFEDistantLightElement",
  // "SVGFEDisplacementMapElement",
  // "SVGFEDiffuseLightingElement",
  // "SVGFEConvolveMatrixElement",
  // "SVGFECompositeElement",
  // "SVGFEComponentTransferElement",
  // "SVGFEColorMatrixElement",
  // "SVGFEBlendElement",
  // "SVGEllipseElement",
  // "SVGElement",
  // "SVGDiscardElement",
  // "SVGDescElement",
  // "SVGDefsElement",
  // "SVGComponentTransferFunctionElement",
  // "SVGClipPathElement",
  // "SVGCircleElement",
  // "SVGAnimationElement",
  // "SVGAnimatedTransformList",
  // "SVGAnimatedString",
  // "SVGAnimatedRect",
  // "SVGAnimatedPreserveAspectRatio",
  // "SVGAnimatedNumberList",
  // "SVGAnimatedNumber",
  // "SVGAnimatedLengthList",
  // "SVGAnimatedLength",
  // "SVGAnimatedInteger",
  // "SVGAnimatedEnumeration",
  // "SVGAnimatedBoolean",
  // "SVGAnimatedAngle",
  // "SVGAnimateTransformElement",
  // "SVGAnimateMotionElement",
  // "SVGAnimateElement",
  // "SVGAngle",
  // "SVGAElement",
  // "Response",
  // "ResizeObserverEntry",
  // "ResizeObserver",
  // "Request",
  // "ReportingObserver",
  // "ReadableStream",
  // "Range",
  // "RadioNodeList",
  // "PromiseRejectionEvent",
  // "ProgressEvent",
  // "ProcessingInstruction",
  // "PopStateEvent",
  // "PointerEvent",
  // "PerformanceTiming",
  // "PerformanceServerTiming",
  // "PerformanceResourceTiming",
  // "PerformancePaintTiming",
  // "PerformanceObserverEntryList",
  // "PerformanceObserver",
  // "PerformanceNavigationTiming",
  // "PerformanceNavigation",
  // "PerformanceMeasure",
  // "PerformanceMark",
  // "PerformanceLongTaskTiming",
  // "PerformanceEntry",
  // "Performance",
  // "PageTransitionEvent",
  // "NodeList",
  // "NodeIterator",
  // "NodeFilter",
  "Node",
  "Navigator",
  // "NamedNodeMap",
  // "MutationRecord",
  // "MutationObserver",
  // "MutationEvent",
  // "MouseEvent",
  // "MessagePort",
  // "MessageEvent",
  // "MessageChannel",
  // "MediaQueryListEvent",
  // "MediaQueryList",
  // "MediaList",
  // "MediaError",
  "Location",
  // "KeyboardEvent",
  // "IntersectionObserverEntry",
  // "IntersectionObserver",
  // "InputEvent",
  // "InputDeviceCapabilities",
  // "ImageData",
  // "ImageBitmap",
  // "IdleDeadline",
  // "History",
  // "Headers",
  // "HashChangeEvent",
  // "HTMLVideoElement",
  // "HTMLUnknownElement",
  // "HTMLUListElement",
  // "HTMLTrackElement",
  // "HTMLTitleElement",
  // "HTMLTimeElement",
  // "HTMLTextAreaElement",
  // "HTMLTemplateElement",
  // "HTMLTableSectionElement",
  // "HTMLTableRowElement",
  // "HTMLTableElement",
  // "HTMLTableColElement",
  // "HTMLTableCellElement",
  // "HTMLTableCaptionElement",
  // "HTMLStyleElement",
  // "HTMLSpanElement",
  // "HTMLSourceElement",
  // "HTMLSlotElement",
  // "HTMLShadowElement",
  // "HTMLSelectElement",
  // "HTMLScriptElement",
  // "HTMLQuoteElement",
  // "HTMLProgressElement",
  // "HTMLPreElement",
  // "HTMLPictureElement",
  // "HTMLParamElement",
  // "HTMLParagraphElement",
  // "HTMLOutputElement",
  // "HTMLOptionsCollection",
  // "Option",
  // "HTMLOptionElement",
  // "HTMLOptGroupElement",
  // "HTMLObjectElement",
  // "HTMLOListElement",
  // "HTMLModElement",
  // "HTMLMeterElement",
  // "HTMLMetaElement",
  // "HTMLMenuElement",
  // "HTMLMediaElement",
  // "HTMLMarqueeElement",
  // "HTMLMapElement",
  // "HTMLLinkElement",
  // "HTMLLegendElement",
  // "HTMLLabelElement",
  // "HTMLLIElement",
  // "HTMLInputElement",
  "Image",
  // "HTMLImageElement",
  // "HTMLIFrameElement",
  // "HTMLHtmlElement",
  // "HTMLHeadingElement",
  // "HTMLHeadElement",
  // "HTMLHRElement",
  // "HTMLFrameSetElement",
  // "HTMLFrameElement",
  // "HTMLFormElement",
  // "HTMLFormControlsCollection",
  // "HTMLFontElement",
  // "HTMLFieldSetElement",
  // "HTMLEmbedElement",
  // "HTMLElement",
  // "HTMLDocument",
  // "HTMLDivElement",
  // "HTMLDirectoryElement",
  // "HTMLDialogElement",
  // "HTMLDetailsElement",
  // "HTMLDataListElement",
  // "HTMLDataElement",
  // "HTMLDListElement",
  // "HTMLContentElement",
  // "HTMLCollection",
  // "HTMLCanvasElement",
  // "HTMLButtonElement",
  // "HTMLBodyElement",
  // "HTMLBaseElement",
  // "HTMLBRElement",
  // "Audio",
  // "HTMLAudioElement",
  // "HTMLAreaElement",
  // "HTMLAnchorElement",
  // "HTMLAllCollection",
  // "FormData",
  // "FontFaceSetLoadEvent",
  // "FocusEvent",
  // "FileReader",
  // "FileList",
  // "File",
  // "EventTarget",
  // "Event",
  // "ErrorEvent",
  "Element",
  // "DragEvent",
  // "DocumentType",
  // "DocumentFragment",
  "Document",
  // "DataTransferItemList",
  // "DataTransferItem",
  // "DataTransfer",
  // "DOMTokenList",
  // "DOMStringMap",
  // "DOMStringList",
  // "DOMRectReadOnly",
  // "DOMRectList",
  // "DOMRect",
  // "DOMQuad",
  // "DOMPointReadOnly",
  // "DOMPoint",
  // "DOMParser",
  // "DOMMatrixReadOnly",
  // "DOMMatrix",
  // "DOMImplementation",
  // "DOMException",
  // "CustomEvent",
  // "CustomElementRegistry",
  // "CompositionEvent",
  // "Comment",
  // "ClipboardEvent",
  // "CharacterData",
  // "CSSVariableReferenceValue",
  // "CSSUnparsedValue",
  // "CSSUnitValue",
  // "CSSTranslate",
  // "CSSTransformValue",
  // "CSSTransformComponent",
  // "CSSSupportsRule",
  // "CSSStyleValue",
  // "CSSStyleSheet",
  // "CSSStyleRule",
  // "CSSStyleDeclaration",
  // "CSSSkewY",
  // "CSSSkewX",
  // "CSSSkew",
  // "CSSScale",
  // "CSSRuleList",
  // "CSSRule",
  // "CSSRotate",
  // "CSSPositionValue",
  // "CSSPerspective",
  // "CSSPageRule",
  // "CSSNumericValue",
  // "CSSNumericArray",
  // "CSSNamespaceRule",
  // "CSSMediaRule",
  // "CSSMatrixComponent",
  // "CSSMathValue",
  // "CSSMathSum",
  // "CSSMathProduct",
  // "CSSMathNegate",
  // "CSSMathMin",
  // "CSSMathMax",
  // "CSSMathInvert",
  // "CSSKeywordValue",
  // "CSSKeyframesRule",
  // "CSSKeyframeRule",
  // "CSSImportRule",
  // "CSSImageValue",
  // "CSSGroupingRule",
  // "CSSFontFaceRule",
  // "CSS",
  // "CSSConditionRule",
  // "CDATASection",
  // "Blob",
  // "BeforeUnloadEvent",
  // "BarProp",
  // "Attr",
  // "AnimationEvent",
  // "AbortSignal",
  // "AbortController",
  // "WebKitCSSMatrix",
  // "WebKitMutationObserver",
  // "SharedArrayBuffer",
  // "Atomics",
  // "WebAssembly",
  // "MediaCapabilitiesInfo",
  // "OffscreenCanvas",
  // "SharedWorker",
  // "FontFace",
  // "UserActivation",
  // "XSLTProcessor",
  // "TextDecoderStream",
  // "TextEncoderStream",
  // "GamepadHapticActuator",
  "Notification",
  // "OffscreenCanvasRenderingContext2D",
  // "PaymentInstruments",
  // "PaymentManager",
  // "PaymentRequestUpdateEvent",
  // "Permissions",
  // "PermissionStatus",
  // "EnterPictureInPictureEvent",
  // "PictureInPictureWindow",
  // "PushManager",
  // "PushSubscription",
  // "PushSubscriptionOptions",
  // "RemotePlayback",
  // "SpeechSynthesisErrorEvent",
  // "SpeechSynthesisEvent",
  // "SpeechSynthesisUtterance",
  // "CanvasGradient",
  // "CanvasPattern",
  // "Path2D",
  // "WebGL2RenderingContext",
  // "WebGLActiveInfo",
  // "WebGLBuffer",
  // "WebGLFramebuffer",
  // "WebGLProgram",
  // "WebGLQuery",
  // "WebGLRenderbuffer",
  // "WebGLRenderingContext",
  // "WebGLSampler",
  // "WebGLShader",
  // "WebGLShaderPrecisionFormat",
  // "WebGLSync",
  // "WebGLTexture",
  // "WebGLTransformFeedback",
  // "WebGLUniformLocation",
  // "WebGLVertexArrayObject",
  // "BluetoothUUID",
|];

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

let parse_module_name = name => {
  js_globals |> Array.exists(x => x == name) ? "$$" ++ name : name;
};

let parse_lid = (~submodule, lid: longident) => {
  let parts = lid |> Longident.flatten_exn;
  switch (parts) {
  | [] => ParsedData.empty
  | [x] => {
      ParsedData.css:
        switch (submodule) {
        | None => x
        | Some(submodule) => (submodule |> parse_module_name) ++ "." ++ x
        },
      modules: ExternalModuleSet.empty,
    }
  | [hd, ...tl] => {
      ParsedData.css:
        tl
        |> List.fold_left(
             (acc, x) => acc ++ "." ++ (x |> parse_module_name),
             hd |> parse_module_name,
           ),
      modules: hd |> ExternalModuleSet.singleton,
    }
  };
};

let rec parse_function =
        (~args: list((arg_label, expression)), ~submodule, ~loc, lid) => {
  switch (lid.txt) {
  | Lident("|.") => parse_pipe_function(~args, ~data=`first, ~submodule, ~loc)
  | Lident("|>") => parse_pipe_function(~args, ~data=`last, ~submodule, ~loc)
  | _ as lid =>
    lid |> parse_lid(~submodule) |> parse_general_function(~args, ~submodule)
  };
}
and parse_pipe_function =
    (
      ~args: list((arg_label, expression)),
      ~data: [ | `first | `last],
      ~submodule: option(string),
      ~loc: location,
    ) => {
  switch (args) {
  | [
      (Nolabel, _) as l_arg,
      (Nolabel, {pexp_desc: Pexp_ident({txt: r_lid})}),
    ] =>
    r_lid
    |> parse_lid(~submodule)
    |> parse_general_function(~args=[l_arg], ~submodule)

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
    |> parse_lid(~submodule)
    |> parse_general_function(
         ~args=
           switch (data) {
           | `first => [l_arg, ...r_args]
           | `last => List.append(r_args, [l_arg])
           },
         ~submodule,
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
    (
      ~args: list((arg_label, expression)),
      ~submodule: option(string),
      lid: ParsedData.t,
    ) => {
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
                  |> js_arg_from_expr(~submodule)
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
and js_arg_from_expr = (~submodule: option(string), expr: expression) =>
  switch (expr) {
  | {pexp_desc: Pexp_ident({txt: lid})} => Ok(lid |> parse_lid(~submodule))
  | {pexp_desc: Pexp_constant(Pconst_integer(x, _) | Pconst_float(x, _))} =>
    Ok({ParsedData.css: x, modules: ExternalModuleSet.empty})
  | {pexp_desc: Pexp_constant(Pconst_string(x, _, _))} =>
    Ok({ParsedData.css: "\"" ++ x ++ "\"", modules: ExternalModuleSet.empty})
  | {pexp_desc: Pexp_apply({pexp_desc: Pexp_ident(lid)}, args), pexp_loc} =>
    lid |> parse_function(~args, ~submodule, ~loc=pexp_loc)
  | {pexp_loc} =>
    Error({Error.reason: `UnexpectedInterpolation, loc: pexp_loc})
  };

let rec concat_js_expression =
        (
          ~op: string,
          ~loc: location,
          ~res: result(ParsedData.t, Error.t),
          ~submodule: option(string),
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
                ~submodule,
              ),
           r_operands
           |> concat_js_expression(
                ~op=r_op,
                ~loc=r_apply_loc,
                ~res=Ok(ParsedData.empty),
                ~submodule,
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
         |> js_arg_from_expr(~submodule)
         |> Result.flatMap((r: ParsedData.t) =>
              l_operands
              |> concat_js_expression(
                   ~op=l_op,
                   ~loc=l_apply_loc,
                   ~res=Ok(ParsedData.empty),
                   ~submodule,
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
         |> js_arg_from_expr(~submodule)
         |> Result.flatMap((l: ParsedData.t) =>
              r_operands
              |> concat_js_expression(
                   ~op=r_op,
                   ~loc=r_apply_loc,
                   ~res=Ok(ParsedData.empty),
                   ~submodule,
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
         switch (
           l_expr |> js_arg_from_expr(~submodule),
           r_expr |> js_arg_from_expr(~submodule),
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
          ~submodule: option(string),
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
              ~submodule,
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
         let r_lid = r_lid |> parse_lid(~submodule);
         l_operands
         |> concat_css_block(
              ~submodule,
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
              ~submodule,
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
         let r_lid = r_lid |> parse_lid(~submodule);
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
              ~submodule,
            )
         |> Result.flatMap((r: ParsedData.t) =>
              l_operands
              |> concat_css_block(
                   ~submodule,
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
              ~submodule,
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
         |> parse_function(~args=r_operands, ~loc=r_exp_loc, ~submodule)
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
         |> parse_function(~args=r_operands, ~loc=r_exp_loc, ~submodule)
         |> Result.flatMap((r: ParsedData.t) =>
              l_operands
              |> concat_css_block(
                   ~submodule,
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

let generate_module = (~loc, ~submodule, str) => {
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
                         pexp_loc_stack: _,
                         pexp_attributes: _,
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
                         pexp_loc_stack: _,
                         pexp_attributes: _,
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
                    ~submodule,
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
};

let submodule_from_code_path = path => {
  switch (path |> Code_path.submodule_path) {
  | [] => None
  | _ as s_path => Some(s_path |> List.rev |> List.hd)
  };
};

let ext =
  Extension.V3.declare(
    "css",
    Extension.Context.module_expr,
    Ast_pattern.__,
    (~ctxt, payload) => {
      let loc = ctxt |> Expansion_context.Extension.extension_point_loc;
      let submodule =
        ctxt
        |> Expansion_context.Extension.code_path
        |> submodule_from_code_path;

      switch (payload) {
      | PStr(str) => str |> generate_module(~loc, ~submodule)
      | _ => Location.raise_errorf(~loc, "Must be a module")
      };
    },
  );

"rescript-linaria"
|> Ppxlib.Driver.register_transformation(
     ~rules=[Context_free.Rule.extension(ext)],
   );
