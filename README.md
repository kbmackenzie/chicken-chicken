**Chicken Chicken** is an implementation of the [Chicken esoteric language][1]. It compiles Chicken source code to ES2020-compliant JavaScript.

It's written entirely in [CHICKEN Scheme][2]. 🐔

It aims to be **fully compatible** with the original Chicken implementation, while still being:

- **Efficient**: No parsing occurs at runtime.
- **Tiny**: It generates very tiny scripts. For big scripts (> 10KiB), compiler output will be **80%** smaller.
- **Convenient**: The VM will happily produce readable output when asked; no HTML escape codes. You can still demand HTML escape codes by enabling [compatibility mode](#compatibility-mode).

All examples from the original Chicken implementation work properly with Chicken Chicken.

## Install

Chicken Chicken is a self-contained static binary executable. [Installation instructions can be found here](./INSTALL.md).

If you wish to build it from source, instructions can be found [here](./INSTALL.md#building-from-source).

## Usage

You can compile a Chicken script to JavaScript by doing:

```bash
chicken-chicken -o example.js example.chicken
```

You can compile with the `--exec` option and pipe the output to Node to run it:

```bash
chicken-chicken --exec example.chicken | node
```

Chicken Chicken can also generate **ECMAscript modules** and **CommonJS modules**, based on the flags given. [The full documentation can be found here](./docs/chicken-chicken.md). 🐔

<p align="center">
  <img width="512" src="https://media1.tenor.com/m/TIxhU9MkeCQAAAAd/dancing-chicken-dansende-kip.gif" alt="Dancing chicken">
</p>

[1]: https://web.archive.org/web/20180816190122/http://torso.me/chicken
[2]: call-cc.org/
