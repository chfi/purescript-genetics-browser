# Genetics Browser

A working demo of the genome browser can be found [here](https://chfi.github.io/genetics-browser-example/track/).

You need npm 5, as well as the Purescript compiler and build tools. The latter
can be installed with npm:

```shell
npm install -g purescript@"== 0.12.5" spago parcel

```

The browser can be built for production using make, into `/dist/index.js`:

``` shell
make build
```

That produces `./dist/index.js`. Opening `./dist/index.html`
should now display the genome browser.

`index.js` in the root folder can be changed to modify the way the
browser functions are exposed. By default, the PureScript module
`Genetics.Browser.UI` is bound to the global JS variable
`GenomeBrowser`.


For development, start the parcel server with `make start` (or `parcel
index.html`), and compile the PureScript modules with either `spago
build`, start a spago build server with `spago build -w`, or start the
compiler using your IDE.
