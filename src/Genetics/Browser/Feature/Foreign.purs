module Genetics.Browser.Feature.Foreign where

import Prelude
import Data.Maybe.First
import Genetics.Browser.Events.Types
import Data.Argonaut as A
import Control.MonadPlus (guard)
import Data.Argonaut (class DecodeJson, Json, encodeJson, isObject, toObject, (.?), (:=))
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap)
import Data.Foreign (Foreign, F)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Genetics.Browser.Units (class HCoordinate, Bp(..), MBp(..), mbp)

type LocKeys = { locKeys :: Array String
               , chrKeys :: Array String
               , posKeys :: Array String
               }


-- tries keys in the Foldable f until one of them exists on the JObject obj,
-- returning the first value or a Left with an error.
keysDecode :: forall f a.
            Foldable f
         => DecodeJson a
         => JObject
         -> f String
         -> Either String a
keysDecode obj keys = case foldMap (\key -> First $ hush $ obj .? key) keys of
  First Nothing  -> Left $ "Object contained no keys from " <> "todo add error"
  First (Just v) -> Right v
  where hush (Left _)  = Nothing
        hush (Right v) = Just v

parseFeatureLocation :: LocKeys -> JObject -> Either String { chr :: String, pos :: Bp }
parseFeatureLocation { locKeys, chrKeys, posKeys } f = do
  loc <- f `keysDecode` locKeys
  -- pure $ { chr: _, pos: _ } <$> loc `keysDecode` chrKeys <*> (Bp <$> loc `keysDecode` posKeys)
  chr <- loc `keysDecode` chrKeys
  pos <- Bp <$> loc `keysDecode` posKeys
  pure $ { chr, pos }

parseFeatureLocation' :: LocKeys -> JObject -> Either String Event
parseFeatureLocation' ks f = do
  d <- parseFeatureLocation ks f
  let json = encodeJson [ "chr" := d.chr
                        , "pos" := (unwrap d.pos)
                        ]
  case toObject json of
    Nothing -> Left "Error when encoding Location Event"
    Just obj -> Right $ Event { eventData: obj }


type RanKeys = { chrKeys :: Array String
               , minKeys :: Array String
               , maxKeys :: Array String
               }

parseFeatureRange :: RanKeys
                  -> JObject
                  -> Either String { chr :: String, minPos :: Bp, maxPos :: Bp }
parseFeatureRange {chrKeys, minKeys, maxKeys} f = do
  chr <- f `keysDecode` chrKeys
  minPos <- Bp <$> f `keysDecode` minKeys
  maxPos <- Bp <$> f `keysDecode` maxKeys
  pure { chr, minPos, maxPos }


parseFeatureRange' :: RanKeys -> JObject -> Either String Event
parseFeatureRange' ks f = do
  d <- parseFeatureRange ks f
  let json = encodeJson [ "chr" := d.chr
                        , "minPos" := (unwrap d.minPos)
                        , "maxPos" := (unwrap d.maxPos)
                        ]
  case toObject json of
    Nothing -> Left "Error when encoding Range Event"
    Just obj -> Right $ Event { eventData: obj }


type ScoreKeys = { scoreKeys :: Array String }

parseFeatureScore :: ScoreKeys -> JObject -> Either String Event
parseFeatureScore { scoreKeys } f = do
  score <- f `keysDecode` scoreKeys
  let json = encodeJson [ "score" := (score :: Number)
                        ]
  case toObject json of
    Nothing -> Left "Error when encoding Score Event"
    Just obj -> Right $ Event { eventData: obj }
