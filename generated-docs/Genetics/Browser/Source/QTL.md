## Module Genetics.Browser.Source.QTL

#### `QTLFeature`

``` purescript
type QTLFeature = Feature Bp { score :: Number }
```

#### `FetchCallback`

``` purescript
type FetchCallback = Fn4 String Foreign Number Foreign (Aff (ajax :: AJAX) Unit)
```

#### `SourceImpl`

``` purescript
type SourceImpl = forall eff. String -> Number -> Number -> Number -> Foreign -> Foreign -> FetchCallback -> Aff (ajax :: AJAX | eff) Unit
```

#### `Source`

``` purescript
type Source = forall eff. Fn7 String Number Number Number Foreign Foreign FetchCallback (Aff (ajax :: AJAX | eff) Unit)
```

#### `null`

``` purescript
null :: forall a. Nullable a
```

#### `fetch`

``` purescript
fetch :: forall t50903 t50904 t50905 t50906 t50907 t50908 t50909 t50910 t50911. String -> Fn7 String t50911 t50910 t50909 t50908 t50907 (Fn4 String (Nullable t50905) Number (Nullable t50904) t50903) (Eff (exception :: EXCEPTION, ajax :: AJAX | t50906) (Canceler (ajax :: AJAX | t50906)))
```

#### `featureFilter`

``` purescript
featureFilter :: String -> QTLFeature -> Maybe QTLFeature
```

#### `qtlToForeign`

``` purescript
qtlToForeign :: QTLFeature -> Foreign
```

#### `fetchImpl`

``` purescript
fetchImpl :: forall t50804 t50805 t50806 t50807 t50808 t50818 t50870 t50871 t50877. String -> String -> t50804 -> t50805 -> t50806 -> t50807 -> t50808 -> Fn4 String (Nullable t50870) Number (Nullable t50871) t50877 -> Eff (exception :: EXCEPTION, ajax :: AJAX | t50818) (Canceler (ajax :: AJAX | t50818))
```

#### `lineToFeature`

``` purescript
lineToFeature :: Map String String -> Maybe QTLFeature
```


