module Genetics.Browser.Feature.Foreign where

import Prelude
import Data.Argonaut as A
import Data.Argonaut (Json, (.?))
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.Foreign (Foreign, F)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Genetics.Browser.Units (class HCoordinate, Bp(..), CoordRange(..), MBp(..), mbp)


-- this assumes a location has the "chr" and "pos" keys...
-- feels like this is actually a job for lenses
parseFeatureLocation :: String -> JObject -> Either String { chr :: String, pos :: Bp }
parseFeatureLocation locKey f = do
  loc <- f .? locKey
  chr <- loc .? "chr"
  pos <- Bp <$> loc .? "pos"
  pure $ { chr, pos }

parseFeatureRange :: { chrKey :: String, xlKey :: String, xrKey :: String}
                  -> JObject
                  -> Either String { chr :: String, xl :: Bp, xr :: Bp }
parseFeatureRange {chrKey, xlKey, xrKey} f = do
  chr <- f .? chrKey
  xl <- Bp <$> f .? xlKey
  xr <- Bp <$> f .? xrKey
  pure { chr, xl, xr }
