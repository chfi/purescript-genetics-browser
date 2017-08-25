## Module Genetics.Browser.Events.TrackSink

#### `TrackSink`

``` purescript
data TrackSink (rIn :: # Type) (rFun :: # Type) output
```

TrackSink is basically a record of functions which can handle some
input in the form of Variants.
`rin` is the row of types that this can handle,
`rfun` is the row of types that corresponds to the record with functions
`out` is the output type; all functions in `rfun` must have `out` as output

#### `emptyTrackSink`

``` purescript
emptyTrackSink :: forall b. TrackSink () () b
```

#### `appendTrackSink`

``` purescript
appendTrackSink :: forall l a b rIn1 rIn2 rFun1 rFun2. RowLacks l rIn1 => RowLacks l rFun1 => RowCons l (a -> b) rFun1 rFun2 => RowCons l a rIn1 rIn2 => IsSymbol l => SProxy l -> (a -> b) -> TrackSink rIn1 rFun1 b -> TrackSink rIn2 rFun2 b
```

Given a label and function, adds a handler to an existing TrackSink.
The label must not already be handled by the TrackSink.

#### `applyTrackSink`

``` purescript
applyTrackSink :: forall lt a b rIn rFun. Union lt a rIn => TrackSink rIn rFun b -> Variant lt -> b
```

Runs a handler on an input, where the input is a Variant whose row is a subset of the
TrackSink input row.

#### `forkTrackSink`

``` purescript
forkTrackSink :: forall lt a rIn rFun eff env. Union lt a rIn => TrackSink rIn rFun (env -> Eff (avar :: AVAR | eff) Unit) -> env -> BusRW (Variant lt) -> Aff (avar :: AVAR | eff) (Canceler (avar :: AVAR | eff))
```

Forks an Aff process with a Handler that performs effects, using some environment.
For example, `env` could be a Biodalliance instance.


