## Module Genetics.Browser.Types

#### `Point`

``` purescript
type Point = { x :: Number, y :: Number }
```

#### `View`

``` purescript
type View = { viewStart :: Number, scale :: Number, height :: Number, chr :: String }
```

#### `Renderer`

``` purescript
newtype Renderer
  = Renderer (View -> Array Foreign -> Foreign)
```

#### `Quant`

``` purescript
type Quant = { min :: Number, max :: Number }
```

#### `Biodalliance`

``` purescript
data Biodalliance :: Type
```

#### `BD`

``` purescript
data BD :: Effect
```


