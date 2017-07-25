module Test.Track where


import Prelude
import Data.Argonaut (Json)
import Data.Either (isLeft, isRight)
import Data.Maybe (Maybe(..))
import Genetics.Browser.Config.Track (TrackType(..), readTrackType, validateBDConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

foreign import validBDTrack :: Json
foreign import badBDTrack :: Json


spec :: Spec _ Unit
spec = do
  describe "TrackType and TracksMap" do
    it "TrackType is correctly parsed from string" do
      readTrackType "BDTrack" `shouldEqual` Just BDTrack
      readTrackType "CyGraph" `shouldEqual` Just CyGraph
      readTrackType "---" `shouldEqual` Nothing


  describe "Biodalliance track configuration" do
    it "Is valid with a name" do
      isRight (validateBDConfig validBDTrack) `shouldEqual` true
    it "Is invalid without a name" do
      isLeft (validateBDConfig badBDTrack) `shouldEqual` true

  describe "Cytoscape.js graph configuration" do
    it "Must have an elementsUri" do
      fail "Not implemented"
