module Genetics.Browser.Events.TrackManager where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Bus (BusRW)
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

-- mkSourceManager :: SourceManager ()
-- mkSourceManager = SourceManager ?makeBus

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

-- addSource :: ∀ l a r1 r2.
--              TrackSource
--              SourceManager r1

newtype SinkManager (sinks :: # Type) (sinkFuns :: # Type) output = SinkManager (Record sinkFuns)

mkSinkManager = SinkManager {}

-- addSink :: ∀ rIn1 rFun1 o.
--            TrackSink rIn1 rFun1 o
--         -> SinkManager sinks funs o
--         -> SinkManager sinks2 funs2 o
-- addSink sink mng = ?help



getSinks :: ∀ sinks0 sinks sinkFuns l t output rIn0 rIn rFun.
            RowCons l t sinks0 sinks
         => RowCons l t rIn0 rIn
         => IsSymbol l
         => SProxy l
         -> Proxy t
         -> SinkManager sinks sinkFuns output
         -> List (TrackSink rIn rFun output)
getSinks l t (SinkManager sinkFuns) = ?undef


-- a SinkManager can be fed some event (i.e. a label and a value of the appropriate type)
-- to apply the appropriate record to each sink.

-- well, isn't that just sequence_, basically?


-- data TrackSink (rIn :: # Type) (rFun :: # Type) output = TrackSink (Record rFun)
