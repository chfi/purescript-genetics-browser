module Test.Events where


import Prelude

import Data.Argonaut (Json)
import Data.Either (Either(..))
import Genetics.Browser.Events.TrackSink (SinkConfig, TrackSink, makeTrackSink)
import Genetics.Browser.Events.TrackSource (SourceConfig, TrackSource, makeTrackSource, runTrackSource)
import Genetics.Browser.Events.Types (Event)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)


foreign import sinkConfig1 :: SinkConfig String

foreign import rangeSourceConfig :: SourceConfig
foreign import rawRange1 :: Json
foreign import rawRange2 :: Json

foreign import parsedRange1 :: Event
foreign import parsedRange2 :: Event

foreign import badRangeConfig :: SourceConfig


rangeSource :: Either String (TrackSource Event)
rangeSource = makeTrackSource rangeSourceConfig

badRangeSource :: Either String (TrackSource Event)
badRangeSource = makeTrackSource badRangeConfig

trackSink1 :: TrackSink String
trackSink1 = makeTrackSink sinkConfig1

event1 :: Either String (Array Event)
event1 = do
  source <- rangeSource
  pure $ runTrackSource source rawRange1


spec :: Spec _ Unit
spec = do
  describe "Event sources" do
    it "can parse raw events that have the exact set of keys" do
      case rangeSource of
        Left err -> fail $ "failed to parse rangeSource!\n" <> err
        Right rs -> do
          case runTrackSource rs rawRange1 of
            [] -> fail "failed to parse raw event!"

            [ev] -> do
              ev.evData `shouldEqual` parsedRange1.evData
              ev.name `shouldEqual` parsedRange1.name

            _  -> fail "track source with single handler parsed more than one event; impossible!"

    it "can parse raw events that have a superset of keys" do
      case rangeSource of
        Left err -> fail $ "failed to parse rangeSource!" <> err
        Right rs -> do
          case runTrackSource rs rawRange2 of
            [] -> fail "failed to parse raw event!"

            [ev] -> do
              ev.evData `shouldEqual` parsedRange2.evData
              ev.name `shouldEqual` parsedRange2.name

            _  -> fail "track source with single handler parsed more than one event; impossible!"

    it "fails if raw and event templates don't match" do
      case badRangeSource of
        Left  _ -> pure unit
        Right _ -> fail "Accepted config with event values as superset of raw keys!"
