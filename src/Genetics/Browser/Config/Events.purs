module Genetics.Browser.Config.Events where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (throwError)
import Data.Argonaut (Json, _Object, (.?))
import Data.Either (Either)
import Data.Foreign (F, Foreign, readString, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Data.Lens ((^?))
import Data.Maybe (Maybe(..))
import Genetics.Browser.Biodalliance.Types (Biodalliance)
import Genetics.Browser.Cytoscape.Types (Cytoscape)
import Genetics.Browser.Events.TrackSink (SinkConfig)
import Genetics.Browser.Events.TrackSource (SourceConfig)
import Unsafe.Coerce (unsafeCoerce)

parseTrackSourceConfig :: Foreign -> F SourceConfig
parseTrackSourceConfig for = do
  eventName <- readString =<< for ! "eventName"
  eventTemplate <- for ! "eventTemplate"
  rawTemplate <- for ! "rawTemplate"
  pure $ { eventName
         , eventTemplate: unsafeCoerce eventTemplate
         , rawTemplate: unsafeCoerce rawTemplate
         }


-- this is so unsafe
parseTrackSinkConfig :: ∀ a. Foreign -> F (SinkConfig a)
parseTrackSinkConfig for = do
  eventName <- readString =<< for ! "eventName"
  eventFun <- for ! "eventTemplate" >>= unsafeReadTagged "Function"
  pure $ { eventName
         , eventFun: unsafeCoerce eventFun
         }

parseBDTrackSinkConfig :: Foreign
                       -> F (SinkConfig (Biodalliance -> Eff _ Unit))
parseBDTrackSinkConfig f = unsafeCoerce (parseTrackSinkConfig f)


parseCyGraphSinkConfig :: Foreign
                       -> F (SinkConfig (Cytoscape -> Eff _ Unit))
parseCyGraphSinkConfig f = unsafeCoerce (parseTrackSinkConfig f)
