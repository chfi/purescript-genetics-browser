## Module Genetics.Browser.Cytoscape.Types

#### `Cytoscape`

``` purescript
data Cytoscape :: Type
```

The Cytoscape graph and effect for functions that interact with it

#### `CY`

``` purescript
data CY :: Effect
```

#### `Element`

``` purescript
data Element :: Type
```

Cytoscape elements (Edges and Nodes)

#### `CyEvent`

``` purescript
data CyEvent :: Type
```

#### `elementJObject`

``` purescript
elementJObject :: Element -> JObject
```

Return the JSON representation of an element

#### `elementJson`

``` purescript
elementJson :: Element -> Json
```


