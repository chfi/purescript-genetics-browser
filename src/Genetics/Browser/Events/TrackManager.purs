module Genetics.Browser.Events.TrackManager where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Data.Argonaut (Json)
import Data.Array (catMaybes)
import Data.Maybe (Maybe)
import Data.Record.Unsafe (unsafeGet)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, expand)
import Genetics.Browser.Events.TrackSink (TrackSink(..), runTrackSink)
import Genetics.Browser.Events.TrackSource (TrackSource(..), runTrackSource)
import Genetics.Browser.Events.Types (Event)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)



-- | Takes a source and a sink and returns a function that parses raw events
-- | with the TrackSource, and, if applicable, runs them through the TrackSink.
sourceToSink :: forall a.
                TrackSource Event
             -> TrackSink a
             -> Json
             -> Array a
sourceToSink tProd tCons rawEv = catMaybes $ runTrackSink tCons <$> runTrackSource tProd rawEv


-- | A TrackManager connects sources to sinks.
-- | It keeps track of the various events registered,
-- | to ensure that two events with the same name have the same structure.
data TrackManager t b c = TrackManager (StrMap t) (TrackSource b) (TrackSink c)
