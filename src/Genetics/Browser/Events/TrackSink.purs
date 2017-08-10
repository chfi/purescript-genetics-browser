module Genetics.Browser.Events.TrackSink
       ( TrackSink
       , emptyTrackSink
       , appendTrackSink
       , applyTrackSink
       , forkTrackSink
       ) where

import Prelude

import Control.Monad.Aff (Aff, Canceler, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Rec.Class (forever)
import Data.Record (insert)
import Data.Record.Unsafe (unsafeGet)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Type.Row (class RowLacks)
import Unsafe.Coerce (unsafeCoerce)


-- | TrackSink is basically a record of functions which can handle some
-- | input in the form of Variants.
-- | `rin` is the row of types that this can handle,
-- | `rfun` is the row of types that corresponds to the record with functions
-- | `out` is the output type; all functions in `rfun` must have `out` as output
data TrackSink (rIn :: # Type) (rFun :: # Type) output = TrackSink (Record rFun)

emptyTrackSink :: ∀ b.
               TrackSink () () b
emptyTrackSink = TrackSink {}

mkTrackSink :: ∀ l a b rIn rFun.
               RowLacks l ()
            => RowCons l (a -> b) () rFun
            => RowCons l a () rIn
            => IsSymbol l
            => SProxy l
            -> (a -> b)
            -> TrackSink rIn rFun b
mkTrackSink l f = TrackSink $ insert l f {}


-- | Given a label and function, adds a handler to an existing TrackSink.
-- | The label must not already be handled by the TrackSink.
appendTrackSink :: ∀ l a b rIn1 rIn2 rFun1 rFun2.
                   RowLacks l rIn1
                => RowLacks l rFun1
                => RowCons l (a -> b) rFun1 rFun2
                => RowCons l a rIn1 rIn2
                => IsSymbol l
                => SProxy l
                -> (a -> b)
                -> TrackSink rIn1 rFun1 b
                -> TrackSink rIn2 rFun2 b
appendTrackSink l f (TrackSink r) = TrackSink $ insert l f r


-- | Runs a handler on an input, where the input is a Variant whose row is a subset of the
-- | TrackSink input row.
applyTrackSink :: ∀ lt a b rIn rFun.
                  Union lt a rIn
               => TrackSink rIn rFun b
               -> Variant lt
               -> b
applyTrackSink (TrackSink h) v =
  case coerceV v of
    Tuple tag a -> a # unsafeGet tag h
  where coerceV :: ∀ c. Variant lt -> Tuple String c
        coerceV = unsafeCoerce


-- | Forks an Aff process with a Handler that performs effects, using some environment.
-- | For example, `env` could be a Biodalliance instance.
forkTrackSink :: ∀ lt a rIn rFun eff env.
                 Union lt a rIn
              => TrackSink rIn rFun (env -> Eff (avar :: AVAR | eff) Unit)
              -> env
              -> BusRW (Variant lt)
              -> Aff ( avar :: AVAR | eff ) (Canceler ( avar :: AVAR | eff ))
forkTrackSink h env bus = forkAff $ forever do
  val <- Bus.read bus
  liftEff $ applyTrackSink h val $ env
