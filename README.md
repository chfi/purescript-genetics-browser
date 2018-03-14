# Genetics Browser

This project currently consists of two parts:
1. an original genome browser supporting GWAS, annotations, and gene track
2. a Halogen app with Biodalliance and Cytoscape.js

It is also possible to use it as a library for creating Biodalliance-compatible renderers.

A working demo of the genome browser can be found [here](https://chfi.github.io/genetics-browser-example/track/).

## Dependencies

You need npm and Purescript. PS can be installed using npm:
```shell
npm install -g purescript
```

Then you should be able to get the remaining dependencies:
```shell
make deps
```

## Tests
Unit tests and QuickCheck tests can be run with
```shell
make test
```

Visual tests of the SVG and Canvas renderers can be found in Test.Main.
To run them, first compile the test module:
```shell
make test-browser
```
Then open `./test/index.html` in a web browser.


## Purescript genome browser

Build the track demo with:

```shell
make track
```

That produces `./dist/track/app.js`. Opening `./dist/track/index.html`
should now display the genome browser.

Pass `FLAGS=-w` to `make` for rebuilding on source code change.
The `OUT` variable controls output path, and `NAMESPACE` the output bundle name.

For example, compiling the genome browser track demo on code changes:

```shell
make FLAGS=-w OUT=./demo.js NAMESPACE=GenomeBrowser track
```

The output app.js file can be loaded into an HTML file, doing so exposes
the genome browser Track module at a global variable, `GGB` by default.


## Halogen app with Biodalliance & Cytoscape.js

**Important**
This part of the project has not been updated to work with the new genome browser,
and requires a Biodalliance build from [my fork](https://github.com/chfi/dalliance/tree/gwas).
Easiest way is to use [the demo repo](https://github.com/chfi/genetics-browser-example/tree/master/halogen/).

Building a new `app.js` compatible with the demo page is done with:

```shell
make halogen-app
```
By default, output is placed in `./dist/halogen/app.js`, under the namespace `GGB`.


### Configuration

This compiles a bundle to the file "app.js", which can be used by including it in a HTML page,
which exposes the main Halogen container at `GGB`.
Also make sure to include a compatible version Biodalliance
scripts if you are using it, available here: https://github.com/chfi/genetics-browser-example/tree/master/build
```html
<script language="javascript" src="./app.js"></script>
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

`wrapRenderer` and `browser` are helper functions for initializing a
Biodalliance browser, and should always be set to
`WrappedRenderer.wrapRenderer` and `Browser` respectively, which are
exported from `dalliance-all.js`. `tracks` is a map of arrays of track
configurations, with BD tracks under the `BDTrack` key, and Cy.js
graphs under the `CyGraph` key (not yet implemented).

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


#### Renderers
A Renderer needs to implement one single function: glyphifyFeatures:
```purescript
type View = { viewStart :: Number, scale :: Number }

glyphifyFeatures :: forall r eff. View -> Array (Feature r) -> Array (Glyph r eff)
```

Examples can be found under [Genetics.Browser.Renderer](https://github.com/chfi/purescript-genetics-browser/tree/master/src/Genetics/Browser/Renderer)

The coordinate system used is relative and normalized to features, horizontally,
and the canvas, vertically.
