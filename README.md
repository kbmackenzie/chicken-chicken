**Chicken Chicken** is an implementation of the [Chicken esoteric language][1]. It compiles Chicken source code to ES2020-compliant JavaScript.

It's written entirely in [CHICKEN Scheme][2]. 🐔

It aims to be **fully compatible** with the original Chicken implementation, while still being:

- **Efficient**: No parsing occurs at runtime.
- **Tiny**: It generates very tiny scripts. For big scripts (> 10KiB), compiler output will be **80%** smaller.
- **Convenient**: The VM will happily produce readable output when asked; no HTML escape codes. You can still demand HTML escape codes by enabling [compatibility mode](#compatibility-mode).

All examples from the original Chicken implementation work properly with Chicken Chicken.

## Install

Chicken Chicken is a self-contained, static, single binary executable.

Installation instructions can be found [here](./INSTALL.md). 🐔

If you wish to build it from source, instructions can be found [here](./INSTALL.md#building-from-source).

## Usage

You can compile a Chicken script to JavaScript by doing:

```bash
chicken-chicken example.chicken
```

You can also write the output to a file:

```bash
chicken-chicken -o example.js example.chicken
```

You can compile with the `--exec` option and pipe the output to Node to run it:

```bash
chicken-chicken --exec example.chicken | node
```

### ES Module

To generate an ECMAScript module, use the `--esmodule` flag:

```bash
chicken-chicken --esmodule -o example.js example.chicken
```

The generated module exports a function as its **default export**.

```js
import chicken from './example.js';
const output = chicken('your input here');
```

### CommonJS Module

To generate a CommonJS module, use the `--commonjs` flag:

```bash
chicken-chicken --commonjs -o example.js example.chicken
```

The generated module exports a function:

```js
const chicken = require('./example.js')
const output = chicken('your input here');
```

### Usage On The Browser

```bash
# Export as an ES module:
chicken-chicken --esmodule -o hello.js hello.chicken
```

```html
<!-- Include in HTML file: -->
<script type="module" src="hello.js"></script>
```

## Documentation

The full documentation can be found [here](./docs/chicken-chicken.md).

## Compatibility Mode

Chicken Chicken has a few extensions from the [original Chicken implementation][1] enabled by default—namely, the `BBQ` instruction generates a single character instead of an HTML escape code. To disable that extension and generate fully Chicken-compliant code, use **compatibility mode**.

To enable compatibility mode, use the `--compat`/`-c` flag when compiling.

All examples from the original Chicken implementation work fine without compatibility mode—except for the *"99 chickens" example, which will produce slightly malformed output without compatibility mode, as it relies on HTML escape codes.

[1]: https://web.archive.org/web/20180816190122/http://torso.me/chicken
[2]: call-cc.org/
