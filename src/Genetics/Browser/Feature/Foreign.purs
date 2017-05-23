module Genetics.Browser.Feature.Foreign where

import Prelude
import Data.Maybe.First
import Data.Argonaut as A
import Data.Argonaut (class DecodeJson, Json, (.?))
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
  chr <- loc `keysDecode` chrKeys
  pos <- Bp <$> loc `keysDecode` posKeys
  pure $ { chr, pos }

parseFeatureRange :: { chrKey :: String, xlKey :: String, xrKey :: String}
                  -> JObject
                  -> Either String { chr :: String, xl :: Bp, xr :: Bp }
parseFeatureRange {chrKey, xlKey, xrKey} f = do
  chr <- f .? chrKey
  xl <- Bp <$> f .? xlKey
  xr <- Bp <$> f .? xrKey
  pure { chr, xl, xr }
