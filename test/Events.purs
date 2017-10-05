module Test.Events where


-- import Genetics.Browser.Events.Handler (TrackSink, TrackSource, appendTrackSink, appendTrackSource, applyTrackSink, applyTrackSource, emptyTrackSink, emptyTrackSource)
import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (Json, _Number, _Object, _String, encodeJson, insideOut, toPrims)
import Data.Array (fromFoldable)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe, Maybe(..), fromJust, fromMaybe)
import Data.StrMap as StrMap
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..), fst)
import Data.Variant (Variant, expand, inj, prj)
import Genetics.Browser.Events.TrackSink (SinkConfig, TrackSink(..), makeTrackSink)
import Genetics.Browser.Events.TrackSource (SourceConfig, TrackSource(..), makeTrackSource, runTrackSource)
import Genetics.Browser.Events.Types (Event)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Unsafe.Coerce (unsafeCoerce)


foreign import sinkConfig1 :: SinkConfig String

foreign import rangeSourceConfig :: SourceConfig
foreign import rawRange1 :: Json
foreign import rawRange2 :: Json

foreign import parsedRange1 :: Event
foreign import parsedRange2 :: Event

foreign import badRangeConfig :: SourceConfig


rangeSource :: Maybe (TrackSource Event)
rangeSource = makeTrackSource rangeSourceConfig

badRangeSource :: Maybe (TrackSource Event)
badRangeSource = makeTrackSource badRangeConfig

trackSink1 :: TrackSink String
trackSink1 = makeTrackSink sinkConfig1

event1 :: Maybe (Array Event)
event1 = do
  source <- rangeSource
  pure $ runTrackSource source rawRange1


spec :: Spec _ Unit
spec = do
  describe "Event sources" do
    it "can parse raw events that have the exact set of keys" do
      case rangeSource of
        Nothing -> fail "failed to parse rangeSource!"
        Just rs -> do
          case runTrackSource rs rawRange1 of
            [] -> fail "failed to parse raw event!"

            [ev] -> do
              ev.evData `shouldEqual` parsedRange1.evData
              ev.name `shouldEqual` parsedRange1.name

            _  -> fail "track source with single handler parsed more than one event; impossible!"
    it "can parse raw events that have a superset of keys" do
      case rangeSource of
        Nothing -> fail "failed to parse rangeSource!"
        Just rs -> do
          case runTrackSource rs rawRange2 of
            [] -> fail "failed to parse raw event!"

            [ev] -> do
              ev.evData `shouldEqual` parsedRange2.evData
              ev.name `shouldEqual` parsedRange2.name
              true `shouldEqual` true

            _  -> fail "track source with single handler parsed more than one event; impossible!"

    it "fails if raw and event templates don't match" do
      case badRangeSource of
        Nothing -> true `shouldEqual` true
        Just _  -> fail "Accepted bad config!"
