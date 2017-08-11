module Genetics.Browser.Events.TrackManager where

import Prelude
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Data.Record.Unsafe (unsafeGet)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, expand)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)



-- newtype TrackManager sos sis = TrackManager { sources :: StrMap sos
--                                             , sinks :: StrMap sis
--                                             }


-- A SourceManager simply keeps track of the incoming event bus.
-- we currently just use one event bus since we don't care about
-- from where the events are coming
newtype SourceManager events = SourceManager (BusRW (Variant events))

mkSourceManager :: ∀ eff evs.
                   Aff (avar :: AVAR | eff) (SourceManager evs)
mkSourceManager = do
  bus <- Bus.make
  pure $ SourceManager bus

-- A source can be added as long as the label and type don't conflict
-- with an existing source; i.e. if the label is already in the SourceManager,
-- the types for that label must be the same



-- adding a source is really...
-- providing a producer of the appropriate variants.
--
-- rather, attaching another producer to the bus.
-- the sourcemanager itself is just the bus, after all.

-- Need this since Bus doesn't have a Functor instance
expandBus :: ∀ lt a gt.
             Union lt a gt
          => BusRW (Variant lt)
          -> BusRW (Variant gt)
expandBus = unsafeCoerce

addSource :: ∀ eff events1 evC events2.
             Union events1 evC events2
          => (BusRW (Variant events2) -> Aff _ Unit)
          -> SourceManager events1
          -> Aff _ (SourceManager events2)
addSource cb (SourceManager bus) = cb bus2 *> (pure $ SourceManager bus2)
  where bus2 = expandBus bus


-- A SinkManager knows of all the tracks' respective TrackSinks,
-- i.e. all things that can receive events.
-- so, it receives events from the SourceManager.
-- it then runs the appropriate sinks; basically `applySinkManager` or w/e looks
-- just like `applyTrackSink`
-- the problem might be in the output type... we'll need to unify effects
-- (or more realistically, keep it a type hole~~)
newtype SinkManager (sinks :: # Type) (sinkFuns :: # Type) output = SinkManager (Record sinkFuns)


mkSinkManager :: ∀ a.
                 SinkManager () () a
mkSinkManager = SinkManager {}


applySinkManager :: ∀ lt a b rIn rFun.
                  Union lt a rIn
               => SinkManager rIn rFun b
               -> Variant lt
               -> b
applySinkManager (SinkManager h) v =
  case coerceV v of
    Tuple tag a -> a # unsafeGet tag h
  where coerceV :: ∀ c. Variant lt -> Tuple String c
        coerceV = unsafeCoerce


-- the EventManager must be able to be updated on the fly as tracks get started
-- while still doing its thing...

-- and what, exactly, *is* its "Thing?"
-- that would be actually running the sourcemanager and sinkmanager,
-- i.e. doing applySinkManager (which is in Eff or Aff, remember)
-- while reading from the SourceManager bus.


-- so we probably want something like
newtype EventManager = EventManager { sourceManager :: AVar (SourceManager _)
                                    , sinkManager :: AVar (SinkManager _)}

-- except, again, if we add new sources and sinks to these managers,
-- we the respective types actually change...

-- well, actually, that shouldn't be a problem.
-- the functions to add sinks and sources should have the correct types anyway;
-- the respective managers will have the correct types whether or not they've
-- actually been added yet.

-- which makes another problem apparent; what if a source sends an event that
-- will be handled by a sink that's not ready yet?

-- geez


-- addSink :: ∀ rIn1 rFun1 o.
--            TrackSink rIn1 rFun1 o
--         -> SinkManager sinks funs o
--         -> SinkManager sinks2 funs2 o
-- addSink sink mng = ?help



-- getSinks :: ∀ sinks0 sinks sinkFuns l t output rIn0 rIn rFun.
--             RowCons l t sinks0 sinks
--          => RowCons l t rIn0 rIn
--          => IsSymbol l
--          => SProxy l
--          -> Proxy t
--          -> SinkManager sinks sinkFuns output
--          -> List (TrackSink rIn rFun output)
-- getSinks l t (SinkManager sinkFuns) = ?undef


-- a SinkManager can be fed some event (i.e. a label and a value of the appropriate type)
-- to apply the appropriate record to each sink.

-- well, isn't that just sequence_, basically?


-- data TrackSink (rIn :: # Type) (rFun :: # Type) output = TrackSink (Record rFun)
