## Module Genetics.Browser.Events.TrackSource

#### `TrackSource`

``` purescript
data TrackSource input (rOut :: # Type)
```

An TrackSource has a list of parsers for some input, and can be used to
produce heterogenous lists of parsed values
`in` is the input type; the type of data that this handler can parse
`rout` is the row of types that this can produce

#### `emptyTrackSource`

``` purescript
emptyTrackSource :: forall a. TrackSource a ()
```

#### `appendTrackSource`

``` purescript
appendTrackSource :: forall l a b rOut1 r rOut2. Union rOut1 r rOut2 => RowLacks l rOut1 => RowCons l b rOut1 rOut2 => IsSymbol l => SProxy l -> (a -> Maybe b) -> TrackSource a rOut1 -> TrackSource a rOut2
```

Given a label and parser, adds a handler to an existing TrackSource.
The label must not already be produced by the TrackSource.

#### `applyTrackSource`

``` purescript
applyTrackSource :: forall a rOut. TrackSource a rOut -> a -> List (Variant rOut)
```

Runs an TrackSource, producing a list of all successful parses of the input.


