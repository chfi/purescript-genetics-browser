module Genetics.Browser.Feature.Foreign where

import Prelude
import Data.Argonaut (class DecodeJson, Json, _Object, (.?))
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldMap, foldr)
import Data.Lens (class Wander)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Profunctor.Choice (class Choice)

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
  where hush = either (const Nothing) id


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
