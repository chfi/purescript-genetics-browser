## Module Genetics.Browser.Feature

#### `Feature`

``` purescript
data Feature c r
  = Feature String c c r
```

##### Instances
``` purescript
Bifunctor Feature
```

#### `feature`

``` purescript
feature :: forall c r. HCoordinate c => String -> c -> c -> r -> Feature c r
```

#### `ScreenFeature`

``` purescript
type ScreenFeature r = Feature Number r
```

#### `featureToScreen`

``` purescript
featureToScreen :: forall c. HCoordinate c => c -> c -> (Feature c) ~> ScreenFeature
```

#### `translateFeature`

``` purescript
translateFeature :: forall c. HCoordinate c => c -> (Feature c) ~> (Feature c)
```

#### `scaleFeature`

``` purescript
scaleFeature :: forall c. HCoordinate c => c -> (Feature c) ~> (Feature c)
```


