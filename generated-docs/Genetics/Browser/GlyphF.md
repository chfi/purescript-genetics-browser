## Module Genetics.Browser.GlyphF

#### `GlyphF`

``` purescript
data GlyphF a
  = Circle Point Number a
  | Line Point Point a
  | Rect Point Point a
  | Stroke String a
  | Fill String a
  | Path (Array Point) a
```

DSL for constructing new glyphs

##### Instances
``` purescript
Functor GlyphF
```


