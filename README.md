**Chicken Chicken** is an implementation of the [Chicken esoteric language][1]. It compiles Chicken source code to ES2020-compliant JavaScript.

It's also written entirely in [CHICKEN Scheme][2]. 🐔

It aims to be **fully compatible** with the original Chicken implementation, while still being:

- **Efficient**: No parsing occurs at runtime.
- **Tiny**: It generates very tiny scripts; for big Chicken scripts (> 1MB), the compiler output will be **98% smaller**.
- **A little more convenient**: The VM will happily produce readable output when asked; no HTML escape codes. (**Note:** HTML escape codes can still be generated in **compatibility mode**, enabled with the `--compat`/`-c` flag.)

All examples from the original Chicken implementation work perfectly with Chicken Chicken! ([See observations here.](#compatibility-mode))

## Usage

You can compile a Chicken script to JavaScript by doing:

```bash
chicken-chicken example.chicken
```

You can also write the compiler output to a file:

```bash
chicken-chicken -o example.js example.chicken
```

### ES Modules

To generate an ECMAScript module, use the `--esmodule` flag:

```bash
chicken-chicken --esmodule -o example.js example.chicken
```

The generated module exports one single function:

```js
import chicken from './example.js';
const output = chicken('your input here');
```

### CommonJS Module

To generate a CommonJS module, use the `--commonjs` flag:

```bash
chicken-chicken --commonjs -o example.js example.chicken
```

The generated module exports one single function:

```js
const chicken = require('./example.js')
const output = chicken('your input here');
```

### Usage On The Browsers

```bash
# Compile with a global export:
chicken-chicken hello.chicken --global -o hello.js
```

```html
<!-- Include in HTML file: -->
<script src="hello.js"></script>
```

## Compatibility Mode

To enable compatibility mode, use the `--compat`/`-c` flag.

As of now, only one thing changes in compatibility mode:

- The **BBQ** instruction will generate an HTML escape code instead of a single character.

A few examples from the original Chicken implementation only work properly in compatibility mode.

### 99 Chickens

The [*"99 chickens"* example from the original implementation][1] only works properly in **compatibility mode**, as a part of it explicitly relies on HTML escape codes. If you care at all, use the `--compat` flag when compiling it.

[1]: https://web.archive.org/web/20180816190122/http://torso.me/chicken
[2]: call-cc.org/
