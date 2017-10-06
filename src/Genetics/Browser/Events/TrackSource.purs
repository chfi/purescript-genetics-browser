module Genetics.Browser.Events.TrackSource where
       -- ( TrackSource
       -- , emptyTrackSource
       -- , mkTrackSource
       -- , appendTrackSources
       -- , applyTrackSource
       -- ) where

import Prelude

import Control.Monad.Except (Except, throwError)
import Control.Monad.Except.Trans (catchError, throwError)
import Control.MonadPlus (guard)
import Data.Argonaut (JCursor(JField), Json, JsonPrim, cursorGet, cursorSet, foldJsonString, jsonEmptyObject, primToJson)
import Data.Argonaut as Json
import Data.Array (catMaybes, singleton)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Filterable (filterMap)
import Data.Foldable (all, any, fold, foldMap, foldr)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Genetics.Browser.Events.Types (Event)
import Global.Unsafe (unsafeStringify)

{-
TrackSources are defined with two parts:
1. A JSON structure defining what the event to be produced looks like,
where the leaves are all singleton maps with Strings denoting the types of the respective leaf

2. A JSON structure defining what the raw event looks like,
and how to map it to the produced event.

-}

-- TODO keep track of which event names have been used
newtype TrackSource a = TrackSource (Array (Json -> Maybe a))

derive instance functorTrackSource :: Functor TrackSource
derive instance newtypeTrackSource :: Newtype (TrackSource a) _

instance semigroupTrackSource :: Semigroup (TrackSource a) where
  append (TrackSource s1) (TrackSource s2) = TrackSource (s1 <> s2)

instance monoidTrackSource :: Monoid (TrackSource a) where
  mempty = TrackSource mempty

emptyTrackSource :: ∀ a. TrackSource a
emptyTrackSource = TrackSource mempty


type SourceConfig = { eventName :: String
                    , eventTemplate :: Json
                    , rawTemplate :: Json
                    }


type ValueCursor = { cursor :: JCursor
                   , name :: String
                   , vType :: String
                   }

type RawCursor = { cursor :: JCursor
                 , name :: String
                 }


parseTemplatePath :: Tuple JCursor JsonPrim
                  -> Either String ValueCursor
parseTemplatePath (Tuple cursor t) =
  lmap (append $ "Error when parsing template path " <> unsafeStringify t) do

    vType <- foldJsonString (Left "Template value type is not a JSON String") pure
             $ primToJson t

    name <- note "A piece of the template path is not a string"
            $ case Json.insideOut cursor of
              JField s _ -> Just s
              _          -> Nothing

    pure { cursor, name, vType }


parseTemplateConfig :: Json -> Either String (Array ValueCursor)
parseTemplateConfig = traverse parseTemplatePath <<< Array.fromFoldable <<< Json.toPrims


parseRawTemplatePath :: Tuple JCursor JsonPrim
                     -> Either String RawCursor
parseRawTemplatePath (Tuple cursor t) =
  note ("Error when parsing raw template path for value: " <> unsafeStringify t) do
    name <- Json.toString $ primToJson t
    pure { cursor, name }


parseRawTemplateConfig :: Json
                       -> Either String (Array RawCursor)
parseRawTemplateConfig = traverse parseRawTemplatePath <<< Array.fromFoldable <<< Json.toPrims


fillTemplate :: Array ValueCursor -> StrMap Json -> Maybe Json
fillTemplate t vs = foldr f (Just jsonEmptyObject) t
  where f :: ValueCursor -> Maybe Json -> Maybe Json
        f _ Nothing  = Nothing
        f ec (Just j) = do
          val <- StrMap.lookup ec.name vs
          cursorSet ec.cursor val j


parseRawEvent :: Array RawCursor -> Json -> Maybe (StrMap Json)
parseRawEvent paths json =
  StrMap.fromFoldable <$>
    for paths (\p -> Tuple p.name <$> cursorGet p.cursor json)


-- templates are valid if each name of the event values
-- occur among the raw template cursors at least once
-- (really it's should be only once but TODO)

validateTemplate :: Array RawCursor -> ValueCursor -> Either String ValueCursor
validateTemplate rcs vc =
  if any (\rc -> vc.name == rc.name) rcs
  then pure vc
  else throwError $ "Event property " <> vc.name <> " is not in raw template"


validateTemplates :: Array RawCursor -> Array ValueCursor -> Either String (Array ValueCursor)
validateTemplates rcs = traverse (validateTemplate rcs)


-- validateSourceConfig' :: SourceConfig -> Either String Unit
-- validateSourceConfig' sc = validateTemplates
--                            <$> parseRawTemplateConfig sc.rawTemplate
--                            <*> parseTemplateConfig sc.eventTemplate
--                             *> pure unit

validateSourceConfig :: SourceConfig -> Either String Unit
validateSourceConfig sc = do
  rt <- parseRawTemplateConfig sc.rawTemplate
  et <- parseTemplateConfig sc.eventTemplate
  validateTemplates rt et *> pure unit

-- TODO:
-- instance decodeJsonTrackSource :: DecodeJson (TrackSource Event) where

makeTrackSource :: SourceConfig -> Either String (TrackSource Event)
makeTrackSource sc = do
  rawTemplates <- parseRawTemplateConfig sc.rawTemplate
  eventTemplates <- validateTemplates rawTemplates
                    =<< parseTemplateConfig sc.eventTemplate

  pure $ TrackSource $ singleton $ \rawEvent -> do
    vals <- parseRawEvent rawTemplates rawEvent
    evData <- fillTemplate eventTemplates vals
    pure $ { name: sc.eventName, evData }


runTrackSource :: TrackSource Event -> Json -> Array Event
runTrackSource (TrackSource ts) raw = filterMap (_ $ raw) ts


makeTrackSources :: Array SourceConfig -> Either String (TrackSource Event)
makeTrackSources = map fold <<< traverse makeTrackSource
