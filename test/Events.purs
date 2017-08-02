module Test.Events where


import Genetics.Browser.Events.Handler
import Prelude

import Data.Argonaut (Json)
import Data.Either (isLeft, isRight)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, expand, inj)
import Genetics.Browser.Config.Track (TrackType(..), readTrackType, validateBDConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)


e :: InputHandler () () String
e = emptyHandler

stringH :: InputHandler
           (stringTest :: String)
           (stringTest :: String -> String)
           String
stringH = appendHandler (SProxy :: SProxy "stringTest") id e


numH :: InputHandler
        (numTest :: Number)
        (numTest :: Number -> String)
        String
numH = appendHandler (SProxy :: SProxy "numTest") show e


stringNumH :: InputHandler
              (numTest :: Number, stringTest :: String)
              (numTest :: Number -> String, stringTest :: String -> String)
              String
stringNumH = appendHandler (SProxy :: SProxy "numTest") show stringH
  where f :: Number -> String
        f x = show x


input :: Variant (stringTest :: String)
input = inj (SProxy :: SProxy "stringTest") "hello"

input2 :: Variant (numTest :: Number)
input2 = inj (SProxy :: SProxy "numTest") 123.0


x2 :: String
x2 = applyHandler stringNumH input2

spec :: Spec _ Unit
spec = do
  describe "Event Input Handlers" do
    it "can be extended without affecting the existing handlers" do
      let handler2 :: InputHandler (stringTest :: String, test :: Unit)
                                   (stringTest :: String -> String, test :: Unit -> String) String
          handler2 = appendHandler (SProxy :: SProxy "test") (const "unit") stringH
      applyHandler stringH input `shouldEqual` applyHandler handler2 input

    it "can handle subsets of its input event type" do
      let inputBig :: Variant (stringTest :: String, numTest :: Number)
          inputBig = expand input
      applyHandler stringNumH input `shouldEqual` applyHandler stringNumH inputBig
