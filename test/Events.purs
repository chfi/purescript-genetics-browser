module Test.Events where


-- import Genetics.Browser.Events.Handler (TrackSink, TrackSource, appendTrackSink, appendTrackSource, applyTrackSink, applyTrackSource, emptyTrackSink, emptyTrackSource)
import Genetics.Browser.Events.TrackSink (TrackSink, appendTrackSink, emptyTrackSink, applyTrackSink)
import Genetics.Browser.Events.TrackSource (TrackSource, appendTrackSource, applyTrackSource, emptyTrackSource)
import Prelude
import Data.Argonaut (Json, _Number, _Object, _String)
import Data.Array (fromFoldable)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, expand, inj, prj)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)



eIn :: TrackSink () () String
eIn = emptyTrackSink

stringInH :: TrackSink
           (stringTest :: String)
           (stringTest :: String -> String)
           String
stringInH = appendTrackSink (SProxy :: SProxy "stringTest") id eIn


numInH :: TrackSink
        (numTest :: Number)
        (numTest :: Number -> String)
        String
numInH = appendTrackSink (SProxy :: SProxy "numTest") show eIn

type Inner = { pos :: Number, name :: String }
type Outer = { inner :: Inner, number :: Number }


complexInH :: TrackSink
              (recordTest :: Outer)
              (recordTest :: Outer -> String)
              String
complexInH = appendTrackSink (SProxy :: SProxy "recordTest") f eIn
  where f :: Outer -> String
        f { inner, number } = "name: " <> inner.name <> ", number: " <> show number


stringNumInH :: TrackSink
              (numTest :: Number, stringTest :: String)
              (numTest :: Number -> String, stringTest :: String -> String)
              String
stringNumInH = appendTrackSink (SProxy :: SProxy "numTest") show stringInH
  where f :: Number -> String
        f x = show x


input :: Variant (stringTest :: String)
input = inj (SProxy :: SProxy "stringTest") "hello"

input2 :: Variant (numTest :: Number)
input2 = inj (SProxy :: SProxy "numTest") 123.0


eOut :: TrackSource Json ()
eOut = emptyTrackSource

jsonToStringH :: TrackSource Json (stringTest :: String)
jsonToStringH = appendTrackSource (SProxy :: SProxy "stringTest") f eOut
  where f :: Json -> Maybe String
        f json = json ^? _String

jsonToStringNumH :: TrackSource Json ( stringTest :: String
                                       , numTest :: Number )
jsonToStringNumH = appendTrackSource (SProxy :: SProxy "numTest") f jsonToStringH
  where f :: Json -> Maybe Number
        f json = json ^? _Number

jsonToRecordH :: TrackSource Json (recordTest :: Outer)
jsonToRecordH = appendTrackSource (SProxy :: SProxy "recordTest") f eOut
  where f :: Json -> Maybe Outer
        f json = do
                 obj <- json ^? _Object
                 number <- obj ^? ix "number" <<< _Number
                 inner <- obj ^? ix "inner" <<< _Object
                 pos <- inner ^? ix "pos" <<< _Number
                 name <- inner ^? ix "name" <<< _String
                 pure { number, inner: {pos, name} }


foreign import stringJson :: Json
foreign import numberJson :: Json
foreign import recordJson :: Json

record :: Outer
record = { "inner": { pos: 1.23, name: "recordJson" }, "number": 456.0 }


spec :: forall e. Spec e Unit
spec = do
  describe "Event Input Handlers" do
    it "can be extended without affecting the existing handlers" do
      let handler2 :: TrackSink (stringTest :: String, test :: Unit)
                                   (stringTest :: String -> String, test :: Unit -> String) String
          handler2 = appendTrackSink (SProxy :: SProxy "test") (const "unit") stringInH
      applyTrackSink stringInH input `shouldEqual` applyTrackSink handler2 input

    it "can handle subsets of its input event type" do
      let inputBig :: Variant (stringTest :: String, numTest :: Number)
          inputBig = expand input
      applyTrackSink stringNumInH input `shouldEqual` applyTrackSink stringNumInH inputBig


  describe "Event Output Handlers" do
    it "can parse things" do
      let l = fromFoldable $ applyTrackSource jsonToStringH stringJson
      case l of
        [h] -> prj (SProxy :: SProxy "stringTest") h `shouldEqual` Just "this is a string"
        _   -> fail "TrackSource didn't parse string"


    it "can be extended" do

      let lStr1 = fromFoldable $ applyTrackSource jsonToStringH stringJson
          lStr2 = fromFoldable $ applyTrackSource jsonToStringNumH stringJson
          lNum = fromFoldable $ applyTrackSource jsonToStringNumH numberJson

      case lStr2 of
        [h'] -> prj (SProxy :: SProxy "stringTest") h' `shouldEqual` Just "this is a string"
        _    -> fail "Extended TrackSource didn't parse string"

      case lNum of
        [h] -> prj (SProxy :: SProxy "numTest") h `shouldEqual` Just 123.0
        _   -> fail "TrackSource didn't parse number"

  describe "Connecting Input and Output Handlers" do
    it "Input handlers and output handlers can be connected if their rows match" do




      fail "not implemented"

    it "The Input type can be a superset of the Output type" do
      fail "not implemented"
