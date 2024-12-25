**Chicken Chicken** is an implementation of the [Chicken esoteric language][1]. It compiles Chicken source code to ES2020-compliant JavaScript.

It's written entirely in [CHICKEN Scheme][2]. 🐔

## Table of Contents

1. [Basic Usage](#basic-usage)
2. [Export Syntax](#export-syntax)
3. [Command-Line Interface](#command-line-interface)
4. [Compatibility Mode](#compatibility-mode)

## Basic Usage

Chicken Chicken accepts one or more files as arguments, concatenates them in order and compiles everything. By default, the compiler output is written to standard output.

To write the compiler output to a file, use the `-o`/`--output` option:

```bash
chicken-chicken -o hello.js hello.chicken
```

Alternatively, you can compile with the `--exec` option and pipe the output to Node to run your program:

```bash
chicken-chicken --exec example.chicken | node
```

## Export Syntax

Chicken Chicken can generate **ECMAscript modules** and **CommonJS modules** on demand.

Its default behavior is to define a function in the global object named *'chicken'*.

The exported function accepts an optional **string** argument—the input for your Chicken program—and it returns your program's result (the value at the top of the stack when execution has finished).

### ES Module

To generate an ECMAScript module, use the `--esmodule` flag. The generated module exports a function as its **default export**.

```bash
chicken-chicken --esmodule -o example.js example.chicken
```

```js
import chicken from './example.js';
const output = chicken('your input here');
```

### CommonJS Module

To generate a CommonJS module, use the `--commonjs` flag. The generated module exports a function as its export object.

```bash
chicken-chicken --commonjs -o example.js example.chicken
```

```js
const chicken = require('./example.js')
const output = chicken('your input here');
```

### Global Function

The default behavior for the compiler. It defines a `chicken` function in the global object.

```bash
chicken-chicken -o example.js example.chicken
```

```js
const output = globalThis.chicken('your input here');
```

### Usage On The Browser

Chicken Chicken can be used on the browser when exported with either the `--global` or `--esmodule` options.

```bash
# Export as an ES module:
chicken-chicken --esmodule -o hello.js hello.chicken
```

```html
<!-- Include in HTML file: -->
<script type="module" src="hello.js"></script>
```

## Command-Line Interface

```
Usage: chicken-chicken [OPTIONS...] [FILES...]

 -e, --esmodule           Generate ESM-style export
 -c, --commonjs           Generate CommonJS-style export
 -g, --global             Generate global export
 -x, --exec               Generate an IIFE
 -o, --output=PATH        Output path
 -p, --compat             Enable compatibility mode
 -i, --inspect            Inspect instructions and leave
 -h, --help               Show help message
```

## Compatibility Mode

Chicken Chicken has a few extensions from the [original Chicken implementation][1] enabled by default—namely, the `BBQ` instruction generates a single character instead of an HTML escape code. To disable that extension and generate fully Chicken-compliant code, use **compatibility mode**.

To enable compatibility mode, use the `--compat`/`-c` flag when compiling.

All examples from the original Chicken implementation work fine without compatibility mode—except for the *"99 chickens" example, which will produce slightly malformed output without compatibility mode, as it relies on HTML escape codes.

[1]: https://web.archive.org/web/20180816190122/http://torso.me/chicken
[2]: call-cc.org/
