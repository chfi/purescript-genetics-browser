module Genetics.Browser.Feature.Foreign where

import Prelude
import Data.Argonaut (class DecodeJson, Json, _Object, _String, encodeJson, fromNumber, fromString, jsonParser, toNumber, toObject, (.?), (:=))
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Lens (class Wander, folded, preview, traversed, (^..))
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Types (Traversal', Prism')
import Data.List (List)
import Data.Maybe (Maybe(..), fromJust)
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Choice (class Choice)
import Data.StrMap (fromFoldable, keys, singleton)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Genetics.Browser.Events.Types (Event(..))
import Genetics.Browser.Units (Bp(Bp))
import Partial.Unsafe (unsafePartial)

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
-- keysDecode :: Functor f => JObject -> f String -> Either String

keysDecode obj keys = case foldMap (\key -> First $ hush $ obj .? key) keys of
  First Nothing  -> Left $ "Object contained no keys from " <> "todo add error"
  First (Just v) -> Right v
  where hush (Left _)  = Nothing
        hush (Right v) = Just v


-- pick out a key from an object, Maybe.
-- makes it nicer to compose
objIx :: ∀ p.
        Choice p
     => Wander p
     => String -> p Json Json -> p Json Json
objIx k = _Object <<< ix k


-- Deep parsing of Json objects given a foldable of keys
deepObjIx :: ∀ f p.
             Foldable f
          => Choice p
          => Wander p
          => f String
          -> p Json Json
          -> p Json Json
deepObjIx = foldr ((<<<) <<< objIx) id

-- Just for testing
exEl :: Json
exEl = unsafePartial $ fromJust $ case jsonParser
   "{\"data\": { \"id\": \"a\", \"lrsLoc\": {\"chr\": \"Chr11\" } }}" of
  Left e     -> Nothing
  Right json -> Just json

-- Just an example
lrsLocChr = deepObjIx ["data", "lrsLoc", "chr"] <<< _String


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
    Just obj -> Right $ Event obj


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
    Just obj -> Right $ Event obj


type ScoreKeys = { scoreKeys :: Array String }

parseFeatureScore :: ScoreKeys -> JObject -> Either String Event
parseFeatureScore { scoreKeys } f = do
  score <- f `keysDecode` scoreKeys
  let json = encodeJson [ "score" := (score :: Number)
                        ]
  case toObject json of
    Nothing -> Left "Error when encoding Score Event"
    Just obj -> Right $ Event obj


parsePath :: ∀ f. Foldable f => JObject -> f String -> Either String JObject
parsePath obj = foldl (\acc cur -> case acc of
                         Left er -> Left er
                         Right x -> x .? cur) (Right obj)
