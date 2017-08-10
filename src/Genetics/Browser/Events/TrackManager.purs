module Genetics.Browser.Events.TrackManager where

import Prelude

import Control.Monad.Aff.Bus (BusRW)
import Data.List (List)
import Data.Monoid (mempty)
import Data.StrMap (StrMap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant)
import Genetics.Browser.Events.TrackSink (TrackSink)
import Genetics.Browser.Events.TrackSink as Sink
import Genetics.Browser.Events.TrackSource as Source
import Type.Proxy (Proxy(..))



-- newtype TrackManager sos sis = TrackManager { sources :: StrMap sos
--                                             , sinks :: StrMap sis
--                                             }

newtype SourceManager sources = SourceManager (List (BusRW (Variant sources)))

mkSourceManager :: SourceManager ()
mkSourceManager = SourceManager mempty

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
