## Module Genetics.Browser.UI.Cytoscape

#### `State`

``` purescript
type State = { cy :: Maybe Cytoscape, elemsUrl :: String }
```

#### `Query`

``` purescript
data Query a
  = Initialize String a
  | Reset a
  | Filter (Predicate Element) a
```

#### `Output`

``` purescript
data Output
  = SendEvent
  | SendCy Cytoscape
```

#### `Effects`

``` purescript
type Effects eff = (cy :: CY, ajax :: AJAX, console :: CONSOLE, exception :: EXCEPTION, avar :: AVAR | eff)
```

#### `Slot`

``` purescript
data Slot
  = Slot
```

##### Instances
``` purescript
Eq Slot
Ord Slot
```

#### `component`

``` purescript
component :: forall eff. Component HTML Query Unit Output (Aff (Effects eff))
```

#### `cyParseEventLocation`

``` purescript
cyParseEventLocation :: Element -> Maybe Location
```


