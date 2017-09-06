## Module Genetics.Browser.Cytoscape

#### `Layout`

``` purescript
newtype Layout
```

Wrapper for layout names

#### `circle`

``` purescript
circle :: Layout
```

#### `cytoscape`

``` purescript
cytoscape :: forall eff. Maybe HTMLElement -> Maybe JArray -> Eff (cy :: CY | eff) Cytoscape
```

Creates a Cytoscape.js graph instance.
`htmlEl` is the element to place it in; if Nothing, the graph is headless.
`els` is the array of elements to fill the browser with.

#### `unsafeParseCollection`

``` purescript
unsafeParseCollection :: Foreign -> CyCollection Element
```

#### `graphAddCollection`

``` purescript
graphAddCollection :: forall eff. Cytoscape -> CyCollection Element -> Eff (cy :: CY | eff) Unit
```

#### `graphGetCollection`

``` purescript
graphGetCollection :: forall eff. Cytoscape -> Eff (cy :: CY | eff) (CyCollection Element)
```

#### `runLayout`

``` purescript
runLayout :: forall eff. Cytoscape -> Layout -> Eff (cy :: CY | eff) Unit
```

#### `resizeContainer`

``` purescript
resizeContainer :: forall eff. Cytoscape -> Eff (cy :: CY | eff) Unit
```

#### `ParsedEvent`

``` purescript
newtype ParsedEvent
  = ParsedEvent { cy :: Cytoscape, target :: Either Element Cytoscape }
```

Basic wrapper over the Cy.js on-click events

#### `parseEvent`

``` purescript
parseEvent :: CyEvent -> ParsedEvent
```

#### `onEvent`

``` purescript
onEvent :: forall a eff. Cytoscape -> String -> (ParsedEvent -> Eff (cy :: CY | eff) a) -> Eff (cy :: CY | eff) Unit
```

Set a Cy.js event handler
`ev` is the string identifier of the event type

#### `onClick`

``` purescript
onClick :: forall eff. Cytoscape -> (ParsedEvent -> Eff (cy :: CY | eff) Unit) -> Eff (cy :: CY | eff) Unit
```

#### `graphRemoveCollection`

``` purescript
graphRemoveCollection :: forall eff. CyCollection Element -> Eff (cy :: CY | eff) (CyCollection Element)
```

#### `graphRemoveAll`

``` purescript
graphRemoveAll :: forall eff. Cytoscape -> Eff (cy :: CY | eff) Unit
```


