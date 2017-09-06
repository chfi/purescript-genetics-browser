## Module Genetics.Browser.Biodalliance

#### `BrowserConstructor`

``` purescript
data BrowserConstructor :: Type
```

#### `RenderWrapper`

``` purescript
data RenderWrapper :: Type
```

#### `initBD`

``` purescript
initBD :: forall eff. Options Biodalliance -> RenderWrapper -> BrowserConstructor -> (HTMLElement -> Eff (bd :: BD | eff) Biodalliance)
```

Helper function to create a Biodalliance browser instance.
`opts` should contain the BD track configurations (using the `sources` option),
and an optional set of `renderers`
the `RenderWrapper` is exported by Biodalliance as WrappedRenderer.wrapRenderer,
the `BrowserConstructor` is the `Browser` constructor exported by Biodalliance.
Returns a function that takes an element to place the BD instance in,
and places the browser in said element, returning a reference to the instance.

#### `sources`

``` purescript
sources :: Option Biodalliance (Array BDTrackConfig)
```

#### `RendererInfo`

``` purescript
type RendererInfo = { name :: String, renderer :: Renderer, canvasHeight :: Number }
```

#### `renderers`

``` purescript
renderers :: Option Biodalliance (Array RendererInfo)
```

#### `addFeatureListener`

``` purescript
addFeatureListener :: forall t41967 t41968. Biodalliance -> (StrMap Json -> Eff t41968 t41967) -> Eff (bd :: BD | t41968) Unit
```

#### `addInitListener`

``` purescript
addInitListener :: forall eff a. EffFn2 (bd :: BD | eff) Biodalliance (Eff (bd :: BD | eff) a) Unit
```

Add a callback that's run when the browser is initialized.

#### `setLocation`

``` purescript
setLocation :: forall c eff. HCoordinate c => Biodalliance -> Chr -> c -> c -> Eff (bd :: BD | eff) Unit
```

Set the BD viewport to a given chromosome, left-hand edge, and right-hand edge.

#### `scrollView`

``` purescript
scrollView :: forall c eff. HCoordinate c => Biodalliance -> c -> Eff (bd :: BD | eff) Unit
```

Scroll the BD viewport in the current chromosome by some distance.


### Re-exported from Genetics.Browser.Types:

#### `Biodalliance`

``` purescript
data Biodalliance :: Type
```

#### `BD`

``` purescript
data BD :: Effect
```

