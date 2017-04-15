module Genetics.Browser.Source.QTL
       where

import Prelude
import Data.Map as Map
import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, launchAff)
import Data.Array (fromFoldable, mapMaybe)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn4, Fn7, mkFn7, runFn4)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Genetics.Browser.Feature (Feature(..), feature)
import Genetics.Browser.Units (Bp, MBp(MBp), bp)
import Global (readFloat)
import Network.HTTP.Affjax (AJAX, get)
import Text.Parsing.CSV (defaultParsers)
import Text.Parsing.Parser (parseErrorMessage, runParser)
import Unsafe.Coerce (unsafeCoerce)

type QTLFeature = Feature Bp {score :: Number}

                  -- the first Foreign should be Nullable Feature or similar
type FetchCallback = Fn4 String Foreign Number Foreign (Aff (ajax :: AJAX) Unit)

-- Feature should be in this... maybe
type SourceImpl = ∀ eff. String -> Number -> Number
                  -> Number -> Foreign -> Foreign
                  -> FetchCallback -> Aff (ajax :: AJAX | eff) Unit

type Source = ∀ eff. Fn7 String Number Number
                       Number Foreign Foreign
                       FetchCallback (Aff (ajax :: AJAX | eff) Unit)


-- if we have some way of transforming response bodies to features,
-- as well as an URI to get them from, we can make a Source usable by BD.
-- we also need a way to translate chr, min, max (and the other args?)
-- into e.g. URI parameters, or filtering the output based on them...

-- so it's not quite this simple. the Source needs to actually _do_ things
-- based on the parameters, i.e. the Source is actually parametericized on
-- how we fetch the actually data!

-- meaning there's another level of abstraction, or another part that can be pulled out.
-- type AnySource = ∀ r. (String -> Array (Feature r)) -> String -> Source


null :: ∀ a. Nullable a
null = toNullable Nothing

-- fetch :: Source
fetch :: _
fetch uri = mkFn7 $ fetchImpl uri

featureFilter :: String -> QTLFeature -> Maybe QTLFeature
featureFilter chr f@(Feature fChr _ _ _) = if fChr == chr then Just f else Nothing

-- fetchImpl :: ∀ c. HCoordinate c =>
--              _something -> String -> c -> c -> Number? -> _F -> _F -> Callback
fetchImpl :: _
fetchImpl uri chr pmin pmax scale types pool f = launchAff $ do
  res <- get uri
  let parsed = runParser res.response defaultParsers.fileHeaded
      results = map (lineToFeature >=> featureFilter chr) <$> parsed

  -- traceAny "fetching!!!" $ const (pure unit)
  -- traceAny (fromFoldable <$> results) $ const (pure unit)


  case fromFoldable <$> results of
    Left  er -> unsafeCoerce $
                runFn4 f (parseErrorMessage er) null 0.0 null
    Right fs -> unsafeCoerce $ let fs' = mapMaybe id fs -- filtering away Nothings
                in runFn4 f "" (unsafeCoerce fs') 0.0 null


-- not very safe or stable (case sensitive, numbers as strings...)
lineToFeature :: Map String String -> Maybe QTLFeature
lineToFeature m = do
  chr <- Map.lookup "Chr" m
    -- Convert MBp to Bp in a type safe manner
  min <- bp <$> MBp <$> readFloat <$> Map.lookup "Mb" m
  score <- readFloat <$> (Map.lookup "LRS" m <|> Map.lookup "LOD" m)
  pure $ Feature chr min min { score }
  -- pure $ feature { chr: chr, min: min, max: min, score: score }
