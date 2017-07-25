module Genetics.Browser.Config.Track
       ( BDTrackConfig
       , CyGraphConfig
       , TracksMap
       , validateConfigs
       , TrackType(..)
       , readTrackType
       , validateBDConfig
       , validateCyConfig
       )
       where

import Prelude
import Data.Argonaut (Json, _Array, _Object, _String)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Foreign (Foreign, toForeign)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap)
import Data.Traversable (sequence)


newtype TracksMap = TracksMap (StrMap Json)

getConfigs :: TracksMap -> TrackType -> Either String (Array Json)
getConfigs (TracksMap ts) tt =
  maybe (Left "Incorrect trackType") Right $ ts ^? ix (show tt) <<< _Array

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

newtype BDTrackConfig = BDTrackConfig Json

validateBDConfig :: Json -> Either String BDTrackConfig
validateBDConfig json = case json ^? _Object <<< ix "name" of
  Nothing -> Left $ "BD track config does not have a name"
  Just c  -> Right $ BDTrackConfig $ json


newtype CyGraphConfig = CyGraphConfig Json

validateCyConfig :: Json -> Either String CyGraphConfig
validateCyConfig json = case json ^? _Object <<< ix "elementsUri" of
  Nothing -> Left $ "cy graph config does not have an elementsUri"
  Just c  -> Right $ CyGraphConfig $ json



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


validateConfigs :: TracksMap
                -> { bdTracks :: ValidatedConfigs BDTrackConfig
                   , cyGraphs :: ValidatedConfigs CyGraphConfig
                   }
validateConfigs tm = { bdTracks
                     , cyGraphs
                     }
  where bdTracks = foldConfig tm BDTrack validateBDConfig
        cyGraphs = foldConfig tm CyGraph validateCyConfig
