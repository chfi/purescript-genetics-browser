module Genetics.Browser.Config.Track
       ( BDTrackConfig
       , makeBDTrack
       , validateBDConfig
       , CyGraphConfig
       , makeCyTrack
       , validateCyConfig
       , TrackType
       , TracksMap
       , validateConfigs
       )
       where

import Prelude
import Data.Argonaut (Json, _Array, _Object, _String)
import Data.Either (Either(..))
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
-- derive instance showTrackType :: Show TrackType
instance showTrackType :: Show TrackType where
  show = genericShow

readTrackType :: String -> Maybe TrackType
readTrackType "BDTrack" = Just BDTrack
readTrackType "CyGraph" = Just CyGraph
readTrackType _ = Nothing


getTrackType :: Json -> Maybe TrackType
getTrackType json = readTrackType =<< json ^? _Object <<< ix "trackType" <<< _String


newtype BDTrackConfig = BDTrackConfig Foreign

makeBDTrack :: ∀ r. { name :: String | r } -> BDTrackConfig
makeBDTrack = BDTrackConfig <<< toForeign

validateBDConfig :: Json -> Either String BDTrackConfig
validateBDConfig json = case json ^? _Object <<< ix "name" of
  Nothing -> Left $ "BD track config does not have a name"
  Just c  -> Right $ BDTrackConfig $ toForeign json


-- Maybe this should be in another file (one not called 'Track')
newtype CyGraphConfig = CyGraphConfig Foreign

makeCyTrack :: ∀ r. { elementsUri :: String | r } -> CyGraphConfig
makeCyTrack = CyGraphConfig <<< toForeign

-- this and validateBDConfig must be able to be simplified;
-- there are two levels of validation, first checking it's a track at all,
-- then validating the specific track type
validateCyConfig :: Json -> Either String CyGraphConfig
validateCyConfig json = case json ^? _Object <<< ix "elementsUri" of
  Nothing -> Left $ "cy graph config does not have an elementsUri"
  Just c  -> Right $ CyGraphConfig $ toForeign json


-- TODO: combine validateBDConfig and validateCyConfig
-- validateTrackConfig :: Json -> Either String (Either BDTrackConfig CyGraphConfig)
-- validateTrackConfig :: Json -> Either String (Either3 BDTrackConfig PSTrackConfig CyGraphConfig)


foldConfigs :: forall a b.
               (Json -> Either String a)
            -> (Json -> Either String b)
            -> TracksMap
            -> { bdTracks :: Array (Either String a)
               , cyGraphs :: Array (Either String b)
               }
foldConfigs bdFun cyFun tm = { bdTracks, cyGraphs }
  where bdTracks = map (_ >>= bdFun) $ sequence $ getConfigs tm BDTrack
        cyGraphs = map (_ >>= cyFun) $ sequence $ getConfigs tm CyGraph
 -- there's /probably/ a nicer way to d this


validateConfigs :: TracksMap
                -> { bdTracks :: Array (Either String BDTrackConfig)
                   , cyGraphs :: Array (Either String CyGraphConfig)
                   }
validateConfigs = foldConfigs validateBDConfig validateCyConfig
