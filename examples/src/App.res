module Css = AppStyles

@react.component
let make = () =>
  <div className={Css.flex->Cn.append(Css.box)}>
    <span className={Css.text->Cn.append(Css.font)}> {"ReScript"->React.string} </span>
  </div>
