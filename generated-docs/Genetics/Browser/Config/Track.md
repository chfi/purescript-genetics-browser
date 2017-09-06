## Module Genetics.Browser.Config.Track

#### `TracksMap`

``` purescript
newtype TracksMap
```

Contains arrays of track configurations, inedxed by track types (e.g. BD or Cy)

#### `TrackType`

``` purescript
data TrackType
  = BDTrack
  | CyGraph
```

The different types of track configuration

##### Instances
``` purescript
Eq TrackType
Ord TrackType
Generic TrackType _
Show TrackType
```

#### `readTrackType`

``` purescript
readTrackType :: String -> Maybe TrackType
```

#### `BDTrackConfig`

``` purescript
newtype BDTrackConfig
```

Represents a Biodalliance track configuration

#### `validateBDConfig`

``` purescript
validateBDConfig :: Json -> Either String BDTrackConfig
```

Validate a Biodalliance track configuration; currently only checks for the presence of a name

#### `CyGraphConfig`

``` purescript
newtype CyGraphConfig
```

Represents a Cytoscape.js graph configuration

##### Instances
``` purescript
Newtype CyGraphConfig _
```

#### `validateCyConfig`

``` purescript
validateCyConfig :: Json -> Either String CyGraphConfig
```

Validate a Cytoscape.js graph configuration; currently only checks for the presence of a URI
to some JSON-formatted elements

#### `validateConfigs`

``` purescript
validateConfigs :: TracksMap -> { bdTracks :: ValidatedConfigs BDTrackConfig, cyGraphs :: ValidatedConfigs CyGraphConfig }
```

Given a TracksMap of configurations,
Returns the validated configurations and any errors, indexed by track type


