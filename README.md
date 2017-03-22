# Purescript Genetics Browser

# Usage

Make sure you have Purescript, Bower, and Pulp installed:
```shell
npm install -g purescript bower pulp
```

Install dependencies
```shell
bower install
```

Build a bundle for the renderer you want, like this:

```shell
pulp build --main Genetics.Browser.Renderer.GWAS --skip-entry-point --to ./psRenderer.js
```

If you want to bundle several renderers into one, build the Biodalliance module,
which re-exports the relevant functions:
```shell
pulp build --main Genetics.Browser --skip-entry-point --to ./psRenderer.js
```

Then, in the file with the BioDalliance browser configuration, add the renderer script,
before the code that defines the browser, and add the renderer to the configuration:

```html
<script language="javascript" src="../build/dalliance-all.js"></script>

<script language="javascript" src="./psRenderer.js"></script>

<script language="javascript">

var gwasRenderer = WrappedRenderer.wrapRenderer(
    PS["Biodalliance"].gwasGlyphify, 300.0, 0.0);

var qtlRenderer = WrappedRenderer.wrapRenderer(
    PS["Biodalliance"].qtlGlyphify, 300.0, 0.0);

var b = new Browser({

    eximport Data.Generic.RepternalRenderers: { gwasRenderer: gwasRenderer,
                        qtlRenderer: qtlRenderer},

    .. /* rest of config */
})
```

The renderer can then be used in some or all tracks, by setting the `renderer` option
to `'psrenderer'` in the tier config or the browser config, respectively.


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
