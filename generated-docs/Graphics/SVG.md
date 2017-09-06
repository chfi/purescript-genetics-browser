## Module Graphics.SVG

#### `SVGElement`

``` purescript
data SVGElement
```

An SVG element is a name with attributes

##### Instances
``` purescript
Generic SVGElement _
Eq SVGElement
Show SVGElement
```

#### `renderSVG`

``` purescript
renderSVG :: forall f eff. Foldable f => f SVGElement -> Eff (dom :: DOM | eff) Element
```

Renders some SVGElements, returning an HTML element

#### `SVGTransform`

``` purescript
type SVGTransform = { translate :: { x :: Number, y :: Number }, scale :: { x :: Number, y :: Number } }
```

#### `SVGContext`

``` purescript
type SVGContext = { stroke :: String, fill :: String, strokeWidth :: Number, transform :: SVGTransform }
```

The SVG context (incomplete)

#### `SVG`

``` purescript
type SVG a = StateT SVGContext (Writer (Array SVGElement)) a
```

Contains the current state of the SVG context as well as the SVG document as an array of elements

#### `initialSVG`

``` purescript
initialSVG :: SVGContext
```

The empty/initial SVGContext

#### `setStrokeStyle`

``` purescript
setStrokeStyle :: String -> SVG Unit
```

#### `setFillStyle`

``` purescript
setFillStyle :: String -> SVG Unit
```

#### `circle`

``` purescript
circle :: Number -> Number -> Number -> SVG Unit
```

#### `line`

``` purescript
line :: Number -> Number -> Number -> Number -> SVG Unit
```

#### `path`

``` purescript
path :: Array { x :: Number, y :: Number } -> SVG Unit
```

#### `rect`

``` purescript
rect :: Number -> Number -> Number -> Number -> SVG Unit
```

#### `translate`

``` purescript
translate :: Number -> Number -> SVG Unit
```

#### `scale`

``` purescript
scale :: Number -> Number -> SVG Unit
```


