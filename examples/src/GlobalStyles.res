include %css(
  let margin = 20

  let global =
    css`
      :global() {
        * {
          box-sizing: border-box;
        }

        body {
          font-family: ${Font.family};
        }

        #app {
          display: flex;
          flex-flow: column nowrap;
          align-items: center;
          justify-content: center;
          margin: ${margin};
        }
      }
    `
)
