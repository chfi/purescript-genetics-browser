module Genetics.Browser.Events.TrackSource
       ( TrackSource
       , emptyTrackSource
       , appendTrackSource
       , applyTrackSource
       ) where

import Prelude

import Data.List (List, mapMaybe, singleton, (:))
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, expand, inj)
import Type.Row (class RowLacks)


-- | An TrackSource has a list of parsers for some input, and can be used to
-- | produce heterogenous lists of parsed values
-- | `in` is the input type; the type of data that this handler can parse
-- | `rout` is the row of types that this can produce
data TrackSource input (rOut :: # Type) = TrackSource (List (input -> Maybe (Variant rOut)))

emptyTrackSource :: ∀ a.
                    TrackSource a ()
emptyTrackSource = TrackSource mempty


mkTrackSource :: ∀ l a b rOut.
                 RowCons l b () rOut
              => IsSymbol l
              => SProxy l
              -> (a -> Maybe b)
              -> TrackSource a rOut
mkTrackSource l f = TrackSource $ singleton f'
  where f' :: a -> Maybe (Variant rOut)
        f' a = inj l <$> f a


-- | Given a label and parser, adds a handler to an existing TrackSource.
-- | The label must not already be produced by the TrackSource.
appendTrackSource :: ∀ l a b rOut1 r rOut2.
                     Union rOut1 r rOut2
                  => RowLacks l rOut1
                  => RowCons l b rOut1 rOut2
                  => IsSymbol l
                  => SProxy l
                  -> (a -> Maybe b)
                  -> TrackSource a rOut1
                  -> TrackSource a rOut2
appendTrackSource l f (TrackSource h) = TrackSource $ f' : (map <<< map <<< map) expand h
  where f' :: a -> Maybe (Variant rOut2)
        f' a = inj l <$> f a




-- | Runs an TrackSource, producing a list of all successful parses of the input.
applyTrackSource :: ∀ a rOut.
                    TrackSource a rOut
                 -> a
                 -> List (Variant rOut)
applyTrackSource (TrackSource h) a = mapMaybe (\f -> f a) h
