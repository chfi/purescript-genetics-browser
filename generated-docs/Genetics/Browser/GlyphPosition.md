## Module Genetics.Browser.GlyphPosition

#### `GlyphPosition`

``` purescript
newtype GlyphPosition
  = GlyphPos { min :: Number, max :: Number, minY :: Number, maxY :: Number }
```

GlyphPositions are used by Biodalliance to calculate the hitbox for a glyph,
for seeing if the user has clicked on it.
Easily combined using the Monoid instance.

##### Instances
``` purescript
Generic GlyphPosition _
Newtype GlyphPosition _
Eq GlyphPosition
Show GlyphPosition
Semigroup GlyphPosition
Monoid GlyphPosition
```


