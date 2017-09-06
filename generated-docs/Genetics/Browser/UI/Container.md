## Module Genetics.Browser.UI.Container

#### `BDEventEff`

``` purescript
type BDEventEff eff = (console :: CONSOLE, bd :: BD, avar :: AVAR | eff)
```

#### `CyEventEff`

``` purescript
type CyEventEff eff = (console :: CONSOLE, cy :: CY, avar :: AVAR | eff)
```

#### `BDHandlerOutput`

``` purescript
type BDHandlerOutput eff = Biodalliance -> Eff (BDEventEff eff) Unit
```

#### `CyHandlerOutput`

``` purescript
type CyHandlerOutput eff = Cytoscape -> Eff (CyEventEff eff) Unit
```

#### `locationInputBD`

``` purescript
locationInputBD :: TrackSink (location :: Location) _ (BDHandlerOutput _)
```

#### `rangeInputBD`

``` purescript
rangeInputBD :: TrackSink (range :: Range, location :: Location) _ (BDHandlerOutput _)
```

#### `rangeInputCy`

``` purescript
rangeInputCy :: forall eff. TrackSink (range :: Range) _ (CyHandlerOutput _)
```

#### `rangeEventOutputBD`

``` purescript
rangeEventOutputBD :: TrackSource JObject (range :: Range)
```

#### `parseLocationElementCy`

``` purescript
parseLocationElementCy :: Element -> Maybe Location
```

#### `locationEventOutputCy`

``` purescript
locationEventOutputCy :: TrackSource ParsedEvent (location :: Location)
```

#### `subscribeBDEvents`

``` purescript
subscribeBDEvents :: forall r. TrackSource JObject r -> Biodalliance -> BusRW (Variant r) -> Eff _ Unit
```

#### `subscribeCyEvents`

``` purescript
subscribeCyEvents :: forall r. TrackSource ParsedEvent r -> Cytoscape -> BusRW (Variant r) -> Eff _ Unit
```

#### `qtlGlyphify`

``` purescript
qtlGlyphify :: LinePlotConfig -> Renderer
```

#### `gwasGlyphify`

``` purescript
gwasGlyphify :: Renderer
```

#### `Track`

``` purescript
data Track
  = BDTrack
  | CyTrack
```

#### `State`

``` purescript
type State = Unit
```

#### `Query`

``` purescript
data Query a
  = CreateBD (forall eff. HTMLElement -> Eff (bd :: BD | eff) Biodalliance) a
  | PropagateMessage Message a
  | BDScroll Bp a
  | BDJump Chr Bp Bp a
  | CreateCy String a
  | ResetCy a
```

#### `Message`

``` purescript
data Message
  = BDInstance Biodalliance
  | CyInstance Cytoscape
```

#### `ChildSlot`

``` purescript
type ChildSlot = Either2 Slot Slot
```

WithCy Cytoscape

#### `ChildQuery`

``` purescript
type ChildQuery = Query <\/> Query <\/> (Const Void)
```

#### `Effects`

``` purescript
type Effects eff = Effects (Effects eff)
```

#### `component`

``` purescript
component :: forall eff. Component HTML Query Unit Message (Aff (Effects eff))
```

#### `qtlRenderer`

``` purescript
qtlRenderer :: RendererInfo
```

#### `gwasRenderer`

``` purescript
gwasRenderer :: RendererInfo
```

#### `bdOpts`

``` purescript
bdOpts :: Options Biodalliance
```

#### `main`

``` purescript
main :: Foreign -> Eff _ Unit
```


