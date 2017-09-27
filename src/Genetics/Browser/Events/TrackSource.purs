module Genetics.Browser.Events.TrackSource where
       -- ( TrackSource
       -- , emptyTrackSource
       -- , mkTrackSource
       -- , appendTrackSources
       -- , applyTrackSource
       -- ) where

import Prelude

import Data.Argonaut (JCursor(JField), Json, JsonPrim, cursorGet, cursorSet, jsonEmptyObject, primToJson)
import Data.Argonaut as Json
import Data.Array (catMaybes, singleton)
import Data.Array as Array
import Data.Foldable (foldMap, foldr)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Genetics.Browser.Events.Types (Event)

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
                  -> Maybe ValueCursor
parseTemplatePath (Tuple cursor t) = do
  vType <- Json.toString $ primToJson t
  name <- case Json.insideOut cursor of
    JField s _ -> Just s
    _          -> Nothing
  pure { cursor, name, vType }

parseTemplateConfig :: Json -> Maybe (Array ValueCursor)
parseTemplateConfig = traverse parseTemplatePath <<< Array.fromFoldable <<< Json.toPrims


parseRawTemplatePath :: Tuple JCursor JsonPrim
                     -> Maybe RawCursor
parseRawTemplatePath (Tuple cursor t) = do
  name <- Json.toString $ primToJson t
  pure { cursor, name }

parseRawTemplateConfig :: Json
                       -> Maybe (Array RawCursor)
parseRawTemplateConfig = traverse parseRawTemplatePath <<< Array.fromFoldable <<< Json.toPrims


fillTemplate :: Array ValueCursor -> StrMap Json -> Maybe Json
fillTemplate t vs = foldr f (Just jsonEmptyObject) t
  where f :: ValueCursor -> Maybe Json -> Maybe Json
        f _ Nothing  = Nothing
        f ec (Just j) = do
          val <- StrMap.lookup ec.name vs
          cursorSet ec.cursor val j

parseRawEvent :: Array RawCursor -> Json -> Maybe (StrMap Json)
parseRawEvent paths json = do
  vals <- traverse (\path -> map (Tuple path.name) (cursorGet path.cursor json)) paths
  pure $ StrMap.fromFoldable vals


makeTrackSource :: SourceConfig -> Maybe (TrackSource Event)
makeTrackSource sc = do
  -- parse the templates
  eventTemplate <- parseTemplateConfig sc.eventTemplate
  rawTemplate <- parseRawTemplateConfig sc.rawTemplate
  let parseRaw = parseRawEvent rawTemplate
      fillEvent = fillTemplate eventTemplate

  pure $ TrackSource $ singleton $ \rawEvent -> do
    vals <- parseRaw rawEvent
    evData <- fillEvent vals
    pure $ { name: sc.eventName, evData }


runTrackSource :: TrackSource Event -> Json -> Array Event
runTrackSource (TrackSource ts) raw = catMaybes $ map (\f -> f raw) ts


makeTrackSources :: Array SourceConfig -> Maybe (TrackSource Event)
makeTrackSources scs = foldMap makeTrackSource scs
