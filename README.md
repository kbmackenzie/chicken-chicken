**Chicken Chicken** is an implementation of the [Chicken esoteric language][1]. It compiles Chicken source code to ES2020-compliant JavaScript.

It's also written entirely in [CHICKEN Scheme][2]. 🐔

It aims to be **fully compatible** with the original Chicken implementation, while still being:

- **... efficient**: No parsing occurs at runtime.
- **... small**: It generates very tiny scripts; for big Chicken scripts (> 1MB), the compiler output will be **98% smaller**.
- **... convenient**: The VM will happily produce readable output when asked; no HTML escape codes. (**Note:** HTML escape codes can still be generated in **compatibility mode**, enabled with the `--compat`/`-c` flag.)

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

```js
import chicken from './example.js';
const output = chicken('your input here');
```

### CommonJS Module

To generate a CommonJS module, use the `--commonjs` flag:

```bash
chicken-chicken --commonjs -o example.js example.chicken
```

The generated module exports the only function you need:

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

As of now, only one thing changes in compatibility mode:

- The **BBQ** instruction will generate an HTML escape code instead of a single character.

A few examples from the original Chicken implementation only work properly in compatibility mode.

### 99 Chickens

The [*"99 chickens"* example from the original implementation] only works properly in **compatibility mode**, as a part of it explicitly relies on HTML escape codes. If you care at all, use the `--compat` flag when compiling it.

## Building From Source

**Note:** The instructions below are written with Linux in mind. Building this project on Windows requires a [make][4] implementation **and** a POSIX-compliant shell.

To build `chicken-chicken` from source, you're gonna need:

- The [CHICKEN Scheme][3] binaries—preferably version 5.4.0 or higher.
- [Node.js][4] and [NPM][5].

You can build it using [make][7] or [HenHen][6].

### Make

To install the necessary CHICKEN dependencies, build the `chicken-chicken` binary and install it to `~/.local/bin`, run:

```bash
make install
```

To install `chicken-chicken` somewhere else instead (e.g. `/usr/local/bin`), just define PREFIX:

```bash
make PREFIX=/usr/local/bin install
```

### HenHen

To build this project in an isolated environment using [HenHen][3], just run:

```bash
henhen run make
```

To install the `chicken-chicken` binary to `~/.local/bin`, run:

```bash
henhen run install
```

[1]: https://web.archive.org/web/20180816190122/http://torso.me/chicken
[2]: call-cc.org/
[3]: https://code.call-cc.org/
[4]: https://nodejs.org/
[5]: https://www.npmjs.com/
[6]: https://github.com/kbmackenzie/henhen
[7]: https://www.gnu.org/software/make/
