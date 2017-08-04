module Test.Events where


import Genetics.Browser.Events.Handler (InputHandler, OutputHandler, appendInputHandler, appendOutputHandler, applyInputHandler, applyOutputHandler, emptyInputHandler, emptyOutputHandler)
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



eIn :: InputHandler () () String
eIn = emptyInputHandler

stringInH :: InputHandler
           (stringTest :: String)
           (stringTest :: String -> String)
           String
stringInH = appendInputHandler (SProxy :: SProxy "stringTest") id eIn


numInH :: InputHandler
        (numTest :: Number)
        (numTest :: Number -> String)
        String
numInH = appendInputHandler (SProxy :: SProxy "numTest") show eIn

type Inner = { pos :: Number, name :: String }
type Outer = { inner :: Inner, number :: Number }


complexInH :: InputHandler
              (recordTest :: Outer)
              (recordTest :: Outer -> String)
              String
complexInH = appendInputHandler (SProxy :: SProxy "recordTest") f eIn
  where f :: Outer -> String
        f { inner, number } = "name: " <> inner.name <> ", number: " <> show number


stringNumInH :: InputHandler
              (numTest :: Number, stringTest :: String)
              (numTest :: Number -> String, stringTest :: String -> String)
              String
stringNumInH = appendInputHandler (SProxy :: SProxy "numTest") show stringInH
  where f :: Number -> String
        f x = show x


input :: Variant (stringTest :: String)
input = inj (SProxy :: SProxy "stringTest") "hello"

input2 :: Variant (numTest :: Number)
input2 = inj (SProxy :: SProxy "numTest") 123.0


eOut :: OutputHandler Json ()
eOut = emptyOutputHandler

jsonToStringH :: OutputHandler Json (stringTest :: String)
jsonToStringH = appendOutputHandler (SProxy :: SProxy "stringTest") f eOut
  where f :: Json -> Maybe String
        f json = json ^? _String

jsonToStringNumH :: OutputHandler Json ( stringTest :: String
                                       , numTest :: Number )
jsonToStringNumH = appendOutputHandler (SProxy :: SProxy "numTest") f jsonToStringH
  where f :: Json -> Maybe Number
        f json = json ^? _Number

jsonToRecordH :: OutputHandler Json (recordTest :: Outer)
jsonToRecordH = appendOutputHandler (SProxy :: SProxy "recordTest") f eOut
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
      let handler2 :: InputHandler (stringTest :: String, test :: Unit)
                                   (stringTest :: String -> String, test :: Unit -> String) String
          handler2 = appendInputHandler (SProxy :: SProxy "test") (const "unit") stringInH
      applyInputHandler stringInH input `shouldEqual` applyInputHandler handler2 input

    it "can handle subsets of its input event type" do
      let inputBig :: Variant (stringTest :: String, numTest :: Number)
          inputBig = expand input
      applyInputHandler stringNumInH input `shouldEqual` applyInputHandler stringNumInH inputBig


  describe "Event Output Handlers" do
    it "can parse things" do
      let l = fromFoldable $ applyOutputHandler jsonToStringH stringJson
      case l of
        [h] -> prj (SProxy :: SProxy "stringTest") h `shouldEqual` Just "this is a string"
        _   -> fail "OutputHandler didn't parse string"


    it "can be extended" do

      let lStr1 = fromFoldable $ applyOutputHandler jsonToStringH stringJson
          lStr2 = fromFoldable $ applyOutputHandler jsonToStringNumH stringJson
          lNum = fromFoldable $ applyOutputHandler jsonToStringNumH numberJson

      case lStr2 of
        [h'] -> prj (SProxy :: SProxy "stringTest") h' `shouldEqual` Just "this is a string"
        _    -> fail "Extended OutputHandler didn't parse string"

      case lNum of
        [h] -> prj (SProxy :: SProxy "numTest") h `shouldEqual` Just 123.0
        _   -> fail "OutputHandler didn't parse number"

  describe "Connecting Input and Output Handlers" do
    it "Input handlers and output handlers can be connected if their rows match" do




      fail "not implemented"

    it "The Input type can be a superset of the Output type" do
      fail "not implemented"
