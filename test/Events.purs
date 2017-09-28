module Test.Events where


-- import Genetics.Browser.Events.Handler (TrackSink, TrackSource, appendTrackSink, appendTrackSource, applyTrackSink, applyTrackSource, emptyTrackSink, emptyTrackSource)
import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (Json, _Number, _Object, _String)
import Data.Array (fromFoldable)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe, Maybe(..), fromJust, fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.Variant (Variant, expand, inj, prj)
import Genetics.Browser.Events.TrackSink (SinkConfig, TrackSink(..), makeTrackSink)
import Genetics.Browser.Events.TrackSource (SourceConfig, TrackSource(..), makeTrackSource, runTrackSource)
import Genetics.Browser.Events.Types (Event)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)


foreign import sourceConfig1 :: SourceConfig
foreign import sinkConfig1 :: SinkConfig String
foreign import rawEvent1 :: Json

trackSource1 :: Maybe (TrackSource
                       { name :: String
                       , evData :: Json })
trackSource1 = makeTrackSource sourceConfig1

trackSink1 :: TrackSink String
trackSink1 = makeTrackSink sinkConfig1

event1 :: Array Event
event1 = unsafePartial $ fromJust do
  source <- trackSource1
  pure $ runTrackSource source rawEvent1

spec :: Spec _ Unit
spec = do
  describe "Event sources Handlers" do
    it "can parse raw events" do
      traverse_ (liftEff <<< log <<< unsafeStringify) event1
      true `shouldEqual` true
