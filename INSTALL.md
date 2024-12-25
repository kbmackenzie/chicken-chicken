## Pre-Compiled Binaries

The most convenient installation method is using a pre-compiled binary. You can find pre-compiled binaries [here][1].

### Linux

After downloading, simply set executable permissions and copy it to a directory available in your PATH:

```bash
chmod +x chicken-chicken
cp chicken-chicken ~/.local/bin
```

### Windows

After downloading, extract the archive to a directory and add that directory to your PATH. See [this StackOverflow answer][7] for some guidance with that.

## Building From Source

**Note:** The instructions below are written with Linux in mind. 🐔

To build `chicken-chicken` from source, you're gonna need:

- The [CHICKEN Scheme][2] binaries—preferably version 5.4.0 or higher.
- [Node.js][3] and [NPM][4].

You can build it using [make][5] or [HenHen][6].

**Note:** When installing with [make][5], a few CHICKEN Scheme eggs will be globally installed. If you wish to avoid that, use [HenHen][6].

### Make

To install the necessary CHICKEN dependencies, build the `chicken-chicken` binary and install it to `~/.local/bin`, run:

```bash
make install
```

To install `chicken-chicken` somewhere else instead (e.g. `/usr/local/bin`), just define `PREFIX`:

```bash
make PREFIX=/usr/local/bin install
```

### HenHen

To build this project in an isolated environment using [HenHen][6], just run:

```bash
henhen run make
```

To install the `chicken-chicken` binary to `~/.local/bin`, run:

```bash
henhen run install
```

[1]: https://github.com/kbmackenzie/chicken-chicken/releases
[2]: https://code.call-cc.org/
[3]: https://nodejs.org/
[4]: https://www.npmjs.com/
[5]: https://www.gnu.org/software/make/
[6]: https://github.com/kbmackenzie/henhen
[7]: https://stackoverflow.com/questions/44272416/how-to-add-a-folder-to-path-environment-variable-in-windows-10-with-screensho/44272417#44272417
