module Genetics.Browser.Source.QTL
       where

import Prelude

import Text.Parsing.CSV (defaultParsers)
import Text.Parsing.Parser (parseErrorMessage, runParser)
import Data.Map as Map
import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff)
import Data.Array (fromFoldable, mapMaybe)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Null (writeNull)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Genetics.Browser.Feature (Feature(..))
import Global (readFloat)
import Network.HTTP.Affjax (AJAX, get)
import Unsafe.Coerce (unsafeCoerce)

type QTLFeature = Feature (score :: Number)

                  -- the first Foreign should be Nullable Feature or similar
type FetchCallback = ∀ r. Fn4 String Foreign Number Foreign (Aff (ajax :: AJAX | r) Unit)

-- Feature should be in this, and 'r' should be the row in Feature r
type Source r =  String -> Number -> Number
              -> Number -> Foreign -> Foreign
              -> FetchCallback -> Aff (ajax :: AJAX | r) Unit

fetch :: Source ()
fetch chr pmin pmax scale types pool f = do
  res <- get "http://test-gn2.genenetwork.org/api_pre1/qtl/lod2.csv"
  let parsed = runParser res.response defaultParsers.fileHeaded
      results = (map lineToFeature) <$> parsed
  case fromFoldable <$> results of
    Left  er -> unsafeCoerce $
                runFn4 f (parseErrorMessage er) writeNull 0.0 writeNull
    Right fs -> unsafeCoerce $ let fs' = mapMaybe id fs -- filtering away Nothings
                in runFn4 f "features" (unsafeCoerce fs') 0.0 writeNull


-- not very safe or stable (case sensitive, numbers as strings...)
lineToFeature :: Map String String -> Maybe QTLFeature
lineToFeature m = do
  chr <- Map.lookup "Chr" m
  min <- readFloat <$> Map.lookup "Mb" m
  score <- readFloat <$> (Map.lookup "LRS" m <|> Map.lookup "LOD" m)
  pure $ Feature { chr: chr, min: min, max: min, score: score }
