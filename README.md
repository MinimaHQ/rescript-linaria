# rescript-linaria

[ReScript](https://rescript-lang.org) bindings to [Linaria](https://github.com/callstack/linaria).

## About
These bindings are unsafe. It means you won't get typed CSS.

What you'll get:
- Type-safe classnames
- Everything Linaria offers, such as:
  - Static extraction
  - Functions / variables sharing between ReScript and CSS

It works only with ReScript syntax.

## Installation
You need to install and configure Linaria. Please, refer to [their docs](https://github.com/callstack/linaria#installation).

Install these bindings:

```sh
# yarn
yarn add rescript-linaria
# or npm
npm install --save rescript-linaria
```

As it's implemented as PPX, you need to add this to your `bsconfig.json`:

```json
"ppx-flags": [
  "rescript-linaria/ppx"
],
```

See [example](./examples) of its usage with Webpack.

## Usage

### Basic

```rescript
// AppStyles.res
include %css(
  let cn = css`
    display: flex;
    position: relative;
  `
)

// App.res
module Css = AppStyles

@react.component
let make = () => <div className=Css.cn />
```

⚠️⚠️⚠️ Everything inside `css` tagged template **is unsound**, including interpolations. It is 100% unsafe territory. What you type inside this tag goes directly to JS without any background checks. You basically write `%raw` JS, which gets slightly modified by PPX so Linaria can pick it up.


### Interpolations
```rescript
include %css(
  let pad = 5

  let cn = css`
    padding: ${pad}px;
    color: ${Color.text};
  `
)
```

You can interpolate:
- everything that Linaria accepts for interpolation:
  - primitives, such as strings, numbers, etc
  - applications of general functions
  - applications of functions with pipes are also supported

You can't interpolate:
- applications of functions with labeled/optional arguments
- applications of functions with placeholder arguments
- externals (you must bind external to a variable first)
- other ReScript-only things, such as variants

### Placement
It is recommended to place each `%css` module in its own file and use `include`:

```rescript
// AppStyles.res
include %css(
  // your css...
)
```

It is possible to place it in a submodule but some things wouldn't work, such as interpolation of local variables.

```rescript
module Css = %css(
  let pad = 5

  // It will fail at runtime, unfortunately
  let cn = css`
    padding: ${pad}px;
  `
)

@react.component
let make = () => <div className=Css.cn />
```

---
You can find more examples [here](./examples).
