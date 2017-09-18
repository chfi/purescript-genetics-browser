module Genetics.Browser.Events.TrackSource
       ( TrackSource
       , emptyTrackSource
       , mkTrackSource
       , appendTrackSources
       , applyTrackSource
       ) where

import Prelude

import Data.Argonaut (Json)
import Data.List (List, mapMaybe, singleton)
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Genetics.Browser.Events.Types (Event)


newtype TrackSource = TrackSource (List (Json -> Maybe Event))


emptyTrackSource :: TrackSource
emptyTrackSource = TrackSource mempty


mkTrackSource :: String
              -> (Json -> Maybe Json)
              -> TrackSource
mkTrackSource l f = TrackSource $
                    singleton $ map { eventType: l, event: _ } <<< f

appendTrackSources :: TrackSource
                   -> TrackSource
                   -> TrackSource
appendTrackSources (TrackSource s1) (TrackSource s2) = TrackSource (s1 <> s2)


applyTrackSource :: TrackSource
                 -> Json
                 -> List Event
applyTrackSource (TrackSource h) json = mapMaybe (\f -> f json) h
