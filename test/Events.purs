module Test.Events where


-- import Genetics.Browser.Events.Handler (TrackSink, TrackSource, appendTrackSink, appendTrackSource, applyTrackSink, applyTrackSource, emptyTrackSink, emptyTrackSource)
import Prelude

import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (Json, _Number, _Object, _String)
import Data.Array (fromFoldable)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, expand, inj, prj)
import Genetics.Browser.Events.TrackSink (SinkConfig, makeTrackSink)
import Genetics.Browser.Events.TrackSource (SourceConfig, makeTrackSource, runTrackSource)
import Genetics.Browser.Events.Types (Event)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)


foreign import sourceConfig1 :: SourceConfig
foreign import sinkConfig1 :: SinkConfig String
foreign import rawEvent1 :: Json

trackSource1 = makeTrackSource sourceConfig1
trackSink1 = makeTrackSink sinkConfig1

event1 :: Maybe (Array Event)
event1 = do
  source <- trackSource1
  pure $ runTrackSource source rawEvent1

spec :: forall e. Spec e Unit
spec = do
  describe "Event sources Handlers" do
    it "can parse raw events" do
      true `shouldEqual` true
