# Purescript Genetics Browser

# Usage

Make sure you have Purescript installed globally, e.g. using NPM:
```shell
npm install -g purescript
```

Install dependencies
```shell
npm run deps
```

Build a bundle for the renderer you want, like this:

```shell
npm build pgb.js
```

This compiles a bundle to the file "pgb.js", which can be used by including it in a HTML page,
which exposes the main Halogen container at `PS`:
```html
<script language="javascript" src="./pgb.js"></script>
```

Running the main function, which creates the main container, requires wrapping the BD constructor
in a callback which takes a HTML element:
```html
         var mkBd = function(el) {
             return function() {
                var b = new Browser({

                    injectionPoint: el,

                    chr:        '11',
                    cookieKey:  'mouse38',
                    externalRenderers: { gwasRenderer: gwasRenderer },
                    coordSystem: {
                        speciesName: 'Mouse',
                        taxon: 10090,
                        auth: 'GRCm',
                        version: 38,
                        ucscName: 'mm10'
                    },
                    maxWorkers: 0,
                    sources:      [
                                ,{name: 'GWAS',
                                    renderer: "gwasRenderer",
                                    "forceReduction": -1,
                                    bwgURI: 'http://localhost:8080/gwascatalog.bb',
                                }
                    ,
                    ],
                    uiPrefix: '../',
                    fullScreen: true
                });
                return b;
             };
         };
         PS.main(mkBd)();
```

The callback wraps a nullary function (return function() {}), because of how Purescript works.


# Renderers
A Renderer needs to implement one single function: glyphifyFeatures:
```purescript
type View = { viewStart :: Number, scale :: Number }

glyphifyFeatures :: forall r eff. View -> Array (Feature r) -> Array (Glyph r eff)
```

Look at Biodalliance.Renderer.GWAS for an example.

The coordinate system used is relative and normalized to features, horizontally,
and the canvas, vertically.

# Tests
Visual tests of the SVG and Canvas renderers can be found in Test.Main.
To run them, first compile the test module:
```shell
pulp build --main Test.Main --to ./index.js
```
Then open index.html in a web browser.
