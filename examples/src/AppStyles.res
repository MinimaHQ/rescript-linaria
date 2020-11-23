include %css(
  let pad = 5
  let size = 120
  let ten = () => 10
  let ident = x => x
  let sum = (x, y) => x + y

  let global = css`
    :global() {
      html {
        box-sizing: border-box;
      }
    }
  `

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

      @media ${Mq.smallScreen} {
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
    font-family: monospace;
    font-size: ${10->sum(12)}px;
  `
)
