## Module Genetics.Browser.UI.Biodalliance

#### `State`

``` purescript
type State = { bd :: Maybe Biodalliance }
```

#### `Query`

``` purescript
data Query a
  = Scroll Bp a
  | Jump Chr Bp Bp a
  | Initialize (forall eff. HTMLElement -> Eff (bd :: BD | eff) Biodalliance) a
  | InitializeCallback (SubscribeStatus -> a)
```

#### `Message`

``` purescript
data Message
  = Initialized
  | SendBD Biodalliance
```

#### `Effects`

``` purescript
type Effects eff = (avar :: AVAR, bd :: BD, console :: CONSOLE | eff)
```

#### `Slot`

``` purescript
data Slot
  = Slot
```

##### Instances
``` purescript
Eq Slot
Ord Slot
```

#### `component`

``` purescript
component :: forall eff. Component HTML Query Unit Message (Aff (Effects eff))
```


