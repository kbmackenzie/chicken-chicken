## Building From Source

**Note:** The instructions below are written with Linux in mind. Building this project on Windows requires a [make][4] implementation **and** a POSIX-compliant shell.

To build `chicken-chicken` from source, you're gonna need:

- The [CHICKEN Scheme][1] binaries—preferably version 5.4.0 or higher.
- [Node.js][5] and [NPM][6].

You can build it using [make][4] or [HenHen][3].

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

[1]: http://code.call-cc.org/
[2]: https://git-scm.com/
[3]: https://github.com/kbmackenzie/henhen
[4]: https://www.gnu.org/software/make/
[5]: https://nodejs.org/en
[6]: https://www.npmjs.com/
