module Genetics.Browser.Config.Track
       ( BDTrackConfig
       , makeBDTrack
       , validateBDConfig
       , CyGraphConfig
       , makeCyTrack
       , validateCyConfig
       , TrackType
       )
       where

import Prelude
import Data.Argonaut (Json, _Array, _Object, _String)
import Data.Either (Either(..))
import Data.Foreign (Foreign, toForeign)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap)
import Data.Traversable (sequence)


newtype TrackType = TrackType String

newtype TracksMap = TracksMap (StrMap Json)

getConfigs :: TracksMap -> TrackType -> Either String (Array Json)
getConfigs (TracksMap ts) (TrackType tt) =
  maybe (Left "Incorrect trackType") Right $ ts ^? ix tt <<< _Array

-- derive instance newtypeTrackType :: Newtype TrackType _
derive instance eqTrackType :: Eq TrackType
derive instance ordTrackType :: Ord TrackType

bdTrack :: TrackType
bdTrack = TrackType "BDTrack"
cyGraph :: TrackType
cyGraph = TrackType "CyGraph"

getTrackType :: Json -> Maybe TrackType
getTrackType json = case json ^? _Object <<< ix "trackType" <<< _String of
  -- must be better ways of encoding this. this is ripe for programmer error
  Just "BDTrack" -> Just bdTrack
  Just "CyGraph" -> Just cyGraph
  _ -> Nothing


newtype BDTrackConfig = BDTrackConfig Foreign

makeBDTrack :: ∀ r. { trackType :: TrackType, name :: String | r } -> BDTrackConfig
makeBDTrack = BDTrackConfig <<< toForeign

validateBDConfig :: Json -> Either String BDTrackConfig
validateBDConfig json =
  case getTrackType json of
    Nothing -> Left $ "Track has no type"
    Just (TrackType "CyGraph") -> Left $ "Is CyGraph"
    Just (TrackType "BDTrack") -> case json ^? _Object <<< ix "name" of
      Nothing -> Left $ "BD track config does not have a name"
      Just c  -> Right $ BDTrackConfig $ toForeign json
    Just _ -> Left $ "Track has unrecognized type"

-- Maybe this should be in another file (one not called 'Track')
newtype CyGraphConfig = CyGraphConfig Foreign

makeCyTrack :: ∀ r. { trackType :: TrackType, elementsUri :: String | r } -> CyGraphConfig
makeCyTrack = CyGraphConfig <<< toForeign

-- this and validateBDConfig must be able to be simplified;
-- there are two levels of validation, first checking it's a track at all,
-- then validating the specific track type
validateCyConfig :: Json -> Either String CyGraphConfig
validateCyConfig json = do
  case getTrackType json of
    Nothing -> Left $ "Track has no type"
    Just (TrackType "BDTrack") -> Left $ "Is BD track"
    Just (TrackType "CyGraph") -> case json ^? _Object <<< ix "elementsUri" of
      Nothing -> Left $ "cy graph config does not have an elementsUri"
      Just c  -> Right $ CyGraphConfig $ toForeign json
    Just _ -> Left $ "Track has unrecognized type"

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
  where bdTracks = map (_ >>= bdFun) $ sequence $ getConfigs tm bdTrack
        cyGraphs = map (_ >>= cyFun) $ sequence $ getConfigs tm cyGraph
        test :: _
        test = map (sequence >=> bdFun) getConfigs tm bdTrack
 -- there's /probably/ a nicer way to d this


validateConfigs :: TracksMap
                -> { bdTracks :: Array (Either String BDTrackConfig)
                   , cyGraphs :: Array (Either String CyGraphConfig)
                   }
validateConfigs = foldConfigs validateBDConfig validateCyConfig
