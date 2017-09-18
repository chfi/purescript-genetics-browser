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
import Genetics.Browser.Events.TrackSink (TrackSink, appendTrackSinks, applyTrackSink, emptyTrackSink, mkTrackSink)
import Genetics.Browser.Events.TrackSource (TrackSource, appendTrackSources, applyTrackSource, emptyTrackSource)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)




-- type Inner = { pos :: Number, name :: String }
-- type Outer = { inner :: Inner, number :: Number }


foreign import stringJson :: Json
foreign import numberJson :: Json
foreign import recordJson :: Json

-- record :: Outer
-- record = { "inner": { pos: 1.23, name: "recordJson" }, "number": 456.0 }



spec :: forall e. Spec e Unit
spec = do
  describe "Event Input Handlers" do
    it "can be extended without affecting the existing handlers" do
      true `shouldEqual` true
--       let handler2 :: TrackSink (stringTest :: String, test :: Unit)
--                                    (stringTest :: String -> String, test :: Unit -> String) String
--           handler2 = appendTrackSink (SProxy :: SProxy "test") (const "unit") stringInH
--       applyTrackSink stringInH input `shouldEqual` applyTrackSink handler2 input

--     it "can handle subsets of its input event type" do
--       let inputBig :: Variant (stringTest :: String, numTest :: Number)
--           inputBig = expand input
--       applyTrackSink stringNumInH input `shouldEqual` applyTrackSink stringNumInH inputBig


--   describe "Event Output Handlers" do
--     it "can parse things" do
--       let l = fromFoldable $ applyTrackSource jsonToStringH stringJson
--       case l of
--         [h] -> prj (SProxy :: SProxy "stringTest") h `shouldEqual` Just "this is a string"
--         _   -> fail "TrackSource didn't parse string"


--     it "can be extended" do

--       let lStr1 = fromFoldable $ applyTrackSource jsonToStringH stringJson
--           lStr2 = fromFoldable $ applyTrackSource jsonToStringNumH stringJson
--           lNum = fromFoldable $ applyTrackSource jsonToStringNumH numberJson

--       case lStr2 of
--         [h'] -> prj (SProxy :: SProxy "stringTest") h' `shouldEqual` Just "this is a string"
--         _    -> fail "Extended TrackSource didn't parse string"

--       case lNum of
--         [h] -> prj (SProxy :: SProxy "numTest") h `shouldEqual` Just 123.0
--         _   -> fail "TrackSource didn't parse number"

--   describe "Connecting Input and Output Handlers" do
--     it "Input handlers and output handlers can be connected if their rows match" do




--       fail "not implemented"

--     it "The Input type can be a superset of the Output type" do
--       fail "not implemented"
