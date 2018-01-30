module Genetics.Browser.Config.Templates where


import Prelude

import Control.Alternative (empty)
import Data.Argonaut (JCursor(..), Json, JsonPrim(..), _Number, _Object, _String, cursorGet, insideOut, primToJson, runJsonPrim)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Foldable (foldMap)
import Data.Lens (Getter', Traversal, Traversal', over, re, to, traverseOf, traversed, view, viewOn, (^.), (^?))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Newtype (wrap)
import Data.StrMap (StrMap)
import Data.StrMap as SM
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (Variant, inj, prj)
import Genetics.Browser.Types (Bp(..), ChrId(..))

-- | A module for defining JSON-parsers with JSON templates


type JSONPath = { name :: String
                , path :: JCursor
                , contents :: JsonPrim
                }

type TemplatePath i = { name :: String
                      , path :: JCursor
                      , contents :: i
                      }

type Template a = StrMap (TemplatePath a)


type Validated a = V (NonEmptyList String) a


type GWASConfig = { pointPath :: JCursor
                  , scorePath :: JCursor
                  , chrIdPath :: JCursor
                  }


type GWASFeatureRow = ( point :: Bp
                      , chrId :: ChrId
                      , score :: Number )



type GWASFeature = Record GWASFeatureRow


type GWASTrack = { name :: String
                 , url :: String
                 , parser :: Json -> Either String GWASFeature }


gwasItemParse :: String -> Json -> Maybe (Variant GWASFeatureRow)
gwasItemParse name v
  | name == "point" =
      (inj (SProxy :: SProxy "point") <<< wrap) <$> v ^? _Number

  | name == "score" =
      inj (SProxy :: SProxy "score") <$> v ^? _Number

  | name == "chrId" =
      (inj (SProxy :: SProxy "chrId") <<< wrap) <$> v ^? _String

  | otherwise = Nothing


gwasParse :: Template (Variant GWASFeatureRow)
          -> Json
          -> Maybe (Record GWASFeatureRow)
gwasParse tmp j = do
  pointT <- SM.lookup "point" tmp
  scoreT <- SM.lookup "score" tmp
  chrIdT <- SM.lookup "chrId" tmp
  point <- cursorGet pointT.path j >>= prj (SProxy :: SProxy "point")
  score <- cursorGet scoreT.path j >>= prj (SProxy :: SProxy "score")
  chrId <- cursorGet chrIdT.path j >>= prj (SProxy :: SProxy "chrId")
  pure { point, score, chrId }



cursorHead :: JCursor
           -> Maybe String
cursorHead c =
  case insideOut c of
    JField str _ -> pure str
    JIndex i   _ -> pure $ show i
    JCursorTop   -> empty


parseTemplatePath :: forall a.
                     (String -> JsonPrim -> Maybe a)
                  -> (Tuple JCursor JsonPrim)
                  -> Maybe (TemplatePath a)
parseTemplatePath f (Tuple path end) = do
  name <- cursorHead path
  val <- f name end
  pure { name, path, contents: val }


parseTemplate :: forall a.
                 (String -> JsonPrim -> Maybe a)
              -> List (Tuple JCursor JsonPrim)
              -> Template a
parseTemplate p = SM.fromFoldable
                  <<< map (\r@{name} -> Tuple name r)
                  <<< fromMaybe mempty
                  <<< traverse (parseTemplatePath p)


gwasBasicParse :: (List (Tuple JCursor JsonPrim))
               -> Template (Variant GWASFeatureRow)
gwasBasicParse = parseTemplate (\n v -> gwasItemParse n (primToJson v))
