## Module Genetics.Browser.Glyph

#### `Glyph`

``` purescript
type Glyph = Free GlyphF
```

Free monad for constructing glyphs

#### `circle`

``` purescript
circle :: Point -> Number -> Glyph Unit
```

#### `line`

``` purescript
line :: Point -> Point -> Glyph Unit
```

#### `rect`

``` purescript
rect :: Point -> Point -> Glyph Unit
```

#### `stroke`

``` purescript
stroke :: String -> Glyph Unit
```

#### `fill`

``` purescript
fill :: String -> Glyph Unit
```

#### `path`

``` purescript
path :: Array Point -> Glyph Unit
```


