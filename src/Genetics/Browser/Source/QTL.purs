module Genetics.Browser.Source.QTL
       where

import Prelude
import Text.Parsing.CSV
import Text.Parsing.Parser
import Data.Map as Map
import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Array (fromFoldable)
import Data.Foreign (Foreign)
import Data.HTTP.Method (Method(..))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Debug.Trace (traceAny)
import Genetics.Browser.Feature (Feature(..))
import Global (readFloat)
import Network.HTTP.Affjax (affjax, defaultRequest, get)

type QTLFeature = Feature (score :: Number)


-- this needs to be safer...
-- what a mess.
-- fetch :: ∀ a b. String -> Number -> Number -> Number -> Foreign -> Foreign -> (a -> b) -> Aff _ Unit
fetch chr pmin pmax scale types pool f = do
  liftEff $ log "getting in purescript"
  res <- get "http://test-gn2.genenetwork.org/api_pre1/qtl/lod2.csv"
  let parsed = runParser res.response defaultParsers.fileHeaded
      results = (map lineToFeature) <$> parsed
  liftEff $ log $ "GET: " <> res.response
  traceAny ((fromFoldable <$> results) :: _) (\_ -> pure unit)
  f res


-- also not very safe or stable (case sensitive, numbers as strings...)
lineToFeature :: Map String String -> Maybe QTLFeature
lineToFeature m = do
  chr <- Map.lookup "Chr" m
  min <- readFloat <$> Map.lookup "Mb" m
  score <- readFloat <$> (Map.lookup "LRS" m <|> Map.lookup "LOD" m)
  pure $ Feature { chr: chr, min: min, max: min, score: score }
