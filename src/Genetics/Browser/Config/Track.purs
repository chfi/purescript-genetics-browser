module Genetics.Browser.Config.Track
       ( BDTrackConfig
       , CyGraphConfig
       , TracksMap
       , validateConfigs
       , TrackType(..)
       , readTrackType
       , validateBDConfig
       , validateCyConfig
       , readTracksMap
       )
       where

import Prelude

import Data.Argonaut (Json, _Array, _Object, _String)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (all, foldr, null)
import Data.Foreign (F, Foreign, ForeignError(..), fail)
import Data.Foreign.Keys (keys)
import Data.Generic.Rep (class Generic)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.Traversable (sequence)
import Unsafe.Coerce (unsafeCoerce)

-- | Contains arrays of track configurations, inedxed by track types (e.g. BD or Cy)
newtype TracksMap = TracksMap (StrMap Json)


getConfigs :: TracksMap -> TrackType -> Either String (Array Json)
getConfigs (TracksMap ts) tt =
  maybe (Left "Incorrect trackType") Right $ ts ^? ix (show tt) <<< _Array

-- | The different types of track configuration
data TrackType = BDTrack | CyGraph

derive instance eqTrackType :: Eq TrackType
derive instance ordTrackType :: Ord TrackType
derive instance genericTrackType :: Generic TrackType _

instance showTrackType :: Show TrackType where
  show BDTrack = "BDTrack"
  show CyGraph = "CyGraph"

readTrackType :: String -> Maybe TrackType
readTrackType "BDTrack" = Just BDTrack
readTrackType "CyGraph" = Just CyGraph
readTrackType _ = Nothing


-- TODO: combine validateBDConfig and validateCyConfig
-- validateTrackConfig :: Json -> Either String (Either BDTrackConfig CyGraphConfig)
-- validateTrackConfig :: Json -> Either String (Either3 BDTrackConfig PSTrackConfig CyGraphConfig)

-- | Represents a Biodalliance track configuration
newtype BDTrackConfig = BDTrackConfig Json

-- | Validate a Biodalliance track configuration; currently only checks for the presence of a name
validateBDConfig :: Json -> Either String BDTrackConfig
validateBDConfig json = case json ^? _Object <<< ix "name" of
  Nothing -> Left $ "BD track config does not have a name"
  Just c  -> Right $ BDTrackConfig $ json


-- | Represents a Cytoscape.js graph configuration
newtype CyGraphConfig = CyGraphConfig { elementsUri :: String }
derive instance newtypeCyGraphConfig :: Newtype CyGraphConfig _

-- | Validate a Cytoscape.js graph configuration; currently only checks for the presence of a URI
-- | to some JSON-formatted elements
validateCyConfig :: Json -> Either String CyGraphConfig
validateCyConfig json = case json ^? _Object <<< ix "elementsUri"  <<< _String of
  Nothing  -> Left $ "cy graph config does not have an elementsUri"
  Just uri -> Right $ CyGraphConfig { elementsUri: uri }



type ValidatedConfigs a = { errors :: Array String
                          , results :: Array a
                          }

foldErrors :: forall e r
            . Array (Either e r)
           -> { errors :: Array e
              , results :: Array r
              }
foldErrors = foldr (\c er@{errors, results} ->
                                case c of Left  e -> er { errors  = (e : errors)}
                                          Right r -> er { results = (r : results)}
                   ) { errors: [], results: [] }


foldConfig :: forall a
             . TracksMap
            -> TrackType
            -> (Json -> Either String a)
            -> ValidatedConfigs a
foldConfig tm tt f = foldErrors $ map (_ >>= f) $ sequence $ getConfigs tm tt


-- | Given a TracksMap of configurations,
-- | Returns the validated configurations and any errors, indexed by track type
validateConfigs :: TracksMap
                -> { bdTracks :: ValidatedConfigs BDTrackConfig
                   , cyGraphs :: ValidatedConfigs CyGraphConfig
                   }
validateConfigs tm = { bdTracks
                     , cyGraphs
                     }
  where bdTracks = foldConfig tm BDTrack validateBDConfig
        cyGraphs = foldConfig tm CyGraph validateCyConfig


readTracksMap :: Foreign -> F TracksMap
readTracksMap f = do
  tracksKeys <- (map readTrackType) <$> keys f
  when (null tracksKeys) $ fail $ ForeignError "TracksMap is empty"
  when (not $ all isJust tracksKeys) $ fail $ ForeignError "TracksMap keys are not all TrackType"
  pure $ unsafeCoerce f
