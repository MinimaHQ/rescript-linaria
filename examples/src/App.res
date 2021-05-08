include GlobalStyles

module Css = %css(
  let pad = 5
  let size = 200
  let ten = () => 10
  let ident = x => x
  let sum = (x, y) => x + y

  let flex = css`
    display: flex;
    position: relative;
    align-items: center;
    justify-content: center;
  `

  let box =
    css`
      margin: ${ten()}px;
      padding: ${pad}px;
      width: ${size->ident}px;
      height: ${size->ident}px;
      border: 6px solid ${Color.Border.bg->Polished.lighten(
      0.3,
    )};
      background-color: ${Color.bg};
      border-radius: ${Size.md / 2}px;

      &:hover .${text} {
        background-color: blue;
      }

      @media ${Screen.small} {
        width: ${size - 20}px;
        height: ${size - 20}px;

        & .${font} {
          font-size: calc(16px + ${ten()}px - 50%);
        }
      }
    `

  let text = css`
    color: ${Color.text};
  `

  let font = css`
    font-size: ${10->sum(20)}px;
    font-weight: ${Font.bold};
  `

  let touchScreen = css`
    width: ${Screen.Touch.width}px;
  `
)

@react.component
let make = () =>
  <div className={Css.flex->Cn.append(Css.box)}>
    <span className={Css.text->Cn.append(Css.font)}> {"ReScript"->React.string} </span>
  </div>
