# rescript-linaria

[ReScript](https://rescript-lang.org) bindings to [Linaria](https://github.com/callstack/linaria).

## Should I use it?
Most likely, no. After using it on my personal site, I wouldn't recommend it for something more or less critical. This PPX is a big fat hack and it shows.

## About
These bindings are unsafe. It means you won't get typed CSS.

What you'll get:
- Type-safe and auto-completable classnames
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
moodule Css = %css(
  let cn = css`
    display: flex;
    position: relative;
  `
)

@react.component
let make = () => <div className=Css.cn />
```

⚠️⚠️⚠️ Everything inside `css` tagged template **is unsound**, including interpolations. It is 100% unsafe territory. What you type inside this tag goes directly to JS without any background checks. You basically write `%raw` JS, which gets slightly modified by PPX so Linaria can pick it up.


### Interpolations
```rescript
moodule Css = %css(
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
It is required to have exactly 1 `%css` module within 1 ReScript file.

You can place `%css` module either:
- in `.res` module as a submodule, as shown in the examples above
- or in its own file using `include`:

```rescript
// AppStyles.res
include %css(
  // your css...
)
```

---
You can find more examples [here](./examples).
