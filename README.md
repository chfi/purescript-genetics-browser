# Genetics Browser

A working demo of the genome browser can be found [here](https://chfi.github.io/genetics-browser-example/track/).

You need npm 5, as well as the Purescript compiler and build tools. The latter
can be installed with npm:

```shell
npm install -g purescript@"== 0.11.7" pulp psc-package

```

The browser can then be built using make, into the example folder at `./dist/app.js`:

``` shell
make build
```

That produces `./dist/app.js`. Opening `./dist/index.html`
should now display the genome browser.

The output path can be changed with the OUT option:


``` shell
make OUT=otherdist/index.js build
```

Pass `FLAGS=-w` to `make` for rebuilding on source code change.

``` shell
make FLAGS=-w build
```

The output app.js file can be loaded into an HTML file, doing so exposes
the genome browser Track module at a global variable, `GGB` by default.


Unit tests and QuickCheck tests can be run with
```shell
make test
```
