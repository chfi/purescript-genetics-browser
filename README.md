# Purescript Genetics Browser

# Usage

Make sure you have Purescript installed globally, e.g. using NPM:
```shell
npm install -g purescript
npm install
```

Install dependencies
```shell
npm run deps
```

Build a bundle for the renderer you want, like this:

```shell
npm run build-to pgb.js
```

This compiles a bundle to the file "pgb.js", which can be used by including it in a HTML page,
which exposes the main Halogen container at `PS`. Also make sure to include the Biodalliance
scripts if you are using it:
```html
<script language="javascript" src="./pgb.js"></script>
<script language="javascript" src="./dalliance-all.js"></script>
```


The browser requires a configuration to be run, in the form of a JS object. In the PS code
it is defined as a newtype record:
```purescript
newtype BrowserConfig = BrowserConfig { wrapRenderer :: RenderWrapper
                                      , browser :: BrowserConstructor
                                      , tracks :: TracksMap
                                      }
```

`wrapRenderer` and `browser` are helper functions for initializing a Biodalliance browser,
and should always be set to `WrappedRenderer.wrapRenderer` and `Browser` respectively,
which are exported from `dalliance-all.js`. `tracks` is a map of arrays of track configurations,
with BD tracks under the `BDTrack` key, and Cy.js graphs under the `CyGraph` key (not yet implemented).

This is a basic configuration example, with two BD tracks. The last line runs the browser with the config:

```html
<script language="javascript">
  var sources = [
      {
          name: 'Genome',
          twoBitURI: 'http://www.biodalliance.org/datasets/GRCm38/mm10.2bit',
          desc: 'Mouse reference genome build GRCm38',
          tier_type: 'sequence',
          provides_entrypoints: true
      },
      {
          name: 'QTL',
          tier_type: 'qtl',
          renderer: "qtlRenderer",
          uri: 'http://test-gn2.genenetwork.org/api_pre1/qtl/lod2.csv'
      }
  ];

  var bdTrackSourceConfigs = [{ eventName: "range",
                                eventTemplate: { "chr": "Chr",
                                                 "minPos": "Bp",
                                                 "maxPos": "Bp"
                                },
                                rawTemplate: { "segment": "chr",
                                               "min": "minPos",
                                               "max": "maxPos"
                                }
  }];


  var renderers = { "qtlRenderer": PS.qtlRenderer(250) };

  var config = { wrapRenderer: WrappedRenderer.wrapRenderer,
                 browser: Browser,
                 tracks: { BDTrack: sources, CyGraph: [] },
		 eventSources: { bd: [], cy: [] },
		 eventSinks: { bd: [], cy: [] }

  };

  PS.main(config)();
</script>
```

Placing the above in an HTML file containing a div with ID "psgbHolder" should get you a basic browser.
A full example can be found in `example.html` in this repo -- make sure to place the compiled BD files in
the right place before running it.


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

Unit tests and QuickCheck tests can be run with
```shell
pulp test
# or
npm run test
```
