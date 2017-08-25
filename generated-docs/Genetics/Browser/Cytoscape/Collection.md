## Module Genetics.Browser.Cytoscape.Collection

#### `CyCollection`

``` purescript
data CyCollection :: Type -> Type
```

A cytoscape collection of elements

##### Instances
``` purescript
Eq (CyCollection e)
Semigroup (CyCollection e)
Show (CyCollection e)
```

#### `collectionJson`

``` purescript
collectionJson :: forall e. CyCollection e -> JArray
```

Convert a collection to a JSON array of the contained elements' JSON representations

#### `emptyCollection`

``` purescript
emptyCollection :: Cytoscape -> CyCollection Element
```

Create an empty collection

#### `size`

``` purescript
size :: forall e. CyCollection e -> Int
```

Return the number of elements of the collection

#### `contains`

``` purescript
contains :: forall e. CyCollection e -> CyCollection e -> Boolean
```

#### `connectedEdges`

``` purescript
connectedEdges :: forall e. CyCollection e -> CyCollection e
```

Returns the connected edges of the nodes in the given collection

#### `connectedNodes`

``` purescript
connectedNodes :: forall e. CyCollection e -> CyCollection e
```

Returns the nodes of edges in the given collection

#### `sourceNodes`

``` purescript
sourceNodes :: forall e. CyCollection e -> CyCollection e
```

Returns the source-side nodes of the edges in the collection

#### `targetNodes`

``` purescript
targetNodes :: forall e. CyCollection e -> CyCollection e
```

Returns the target-side nodes of the edges in the collection

#### `filter`

``` purescript
filter :: forall e. Predicate e -> CyCollection e -> CyCollection e
```

#### `isNode`

``` purescript
isNode :: Predicate Element
```

#### `isEdge`

``` purescript
isEdge :: Predicate Element
```


