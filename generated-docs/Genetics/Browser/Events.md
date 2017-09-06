## Module Genetics.Browser.Events

#### `Event`

``` purescript
newtype Event r
  = Event (Variant r)
```

#### `Location`

``` purescript
type Location = { chr :: Chr, pos :: Bp }
```

#### `_eventLocation`

``` purescript
_eventLocation :: SProxy "location"
```

#### `EventLocation`

``` purescript
type EventLocation r = (location :: Location | r)
```

#### `handleLocation`

``` purescript
handleLocation :: forall r a. (Location -> a) -> (Variant r -> a) -> Variant (EventLocation r) -> a
```

#### `Range`

``` purescript
type Range = { chr :: Chr, minPos :: Bp, maxPos :: Bp }
```

#### `_eventRange`

``` purescript
_eventRange :: SProxy "range"
```

#### `EventRange`

``` purescript
type EventRange r = (range :: Range | r)
```

#### `handleRange`

``` purescript
handleRange :: forall r a. (Range -> a) -> (Variant r -> a) -> Variant (EventRange r) -> a
```

#### `Score`

``` purescript
type Score = { score :: Number }
```

#### `_eventScore`

``` purescript
_eventScore :: SProxy "score"
```

#### `EventScore`

``` purescript
type EventScore r = (score :: Score | r)
```


