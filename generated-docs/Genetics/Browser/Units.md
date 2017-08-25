## Module Genetics.Browser.Units

#### `Bp`

``` purescript
newtype Bp
  = Bp Number
```

Newtype wrapper for a basepair location

##### Instances
``` purescript
Newtype Bp _
Eq Bp
Ord Bp
Field Bp
EuclideanRing Bp
CommutativeRing Bp
Semiring Bp
Ring Bp
Encode Bp
Decode Bp
Show Bp
HCoordinate Bp
```

#### `_Bp`

``` purescript
_Bp :: Iso' Bp Number
```

#### `_MBp`

``` purescript
_MBp :: Iso' MBp Number
```

#### `_BpMBp`

``` purescript
_BpMBp :: Iso' Bp MBp
```

#### `MBp`

``` purescript
newtype MBp
  = MBp Number
```

Newtype wrapper for a megabasepair location

##### Instances
``` purescript
Newtype MBp _
Eq MBp
Ord MBp
Field MBp
EuclideanRing MBp
CommutativeRing MBp
Semiring MBp
Ring MBp
Encode MBp
Decode MBp
Show MBp
HCoordinate MBp
```

#### `HCoordinate`

``` purescript
class (Field a) <= HCoordinate a  where
  bp :: a -> Bp
  mbp :: a -> MBp
```

##### Instances
``` purescript
HCoordinate Bp
HCoordinate MBp
```

#### `toScreen`

``` purescript
toScreen :: forall c. HCoordinate c => c -> c -> c -> Number
```

#### `Chr`

``` purescript
newtype Chr
  = Chr String
```

Newtype wrapper for a chromosome identifier

##### Instances
``` purescript
Newtype Chr _
Eq Chr
Encode Chr
Decode Chr
Show Chr
```

#### `_Chr`

``` purescript
_Chr :: Iso' Chr String
```


