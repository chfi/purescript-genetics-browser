module Test.Events where


import Genetics.Browser.Events.Handler
import Prelude

import Data.Argonaut (Json)
import Data.Either (isLeft, isRight)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Genetics.Browser.Config.Track (TrackType(..), readTrackType, validateBDConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)



e :: InputHandler () () String
e = emptyHandler

h1 :: InputHandler
      (stringTest :: String)
      (stringTest :: String -> String)
      String
h1 = appendHandler (SProxy :: SProxy "stringTest") f e
  where f :: String -> String
        f x = x

h2 :: InputHandler
      (numTest :: Number, stringTest :: String)
      (numTest :: Number -> String, stringTest :: String -> String)
      String
h2 = appendHandler (SProxy :: SProxy "numTest") f h1
  where f :: Number -> String
        f x = show x


-- input :: Variant (stringTest :: String)
input :: Variant (stringTest :: String, numTest :: Number)
input = inj (SProxy :: SProxy "stringTest") "hello"

input2 :: Variant (stringTest :: String)
input2 = inj (SProxy :: SProxy "stringTest") "hello"

input3 :: Variant (stringTest :: String, numTest :: Number)
input3 = inj (SProxy :: SProxy "numTest") 123.0

-- x :: String
-- x = applyHandler h2 input

x2 :: String
x2 = applyHandler h1 input3

-- spec :: Spec _ Unit
-- spec = do
--   describe "Event Input Handlers" do
--     it "Can be appended to without changing output of already handled labels" do
      -- let h3 :: InputHandler _ _ String
      --     h3 = appendHandler (SProxy :: SProxy "extra") (const "yes") h1
      -- applyHandler h1 input `shouldEqual` applyHandler h2 input
      -- applyHandler h1 input `shouldEqual` applyHandler h3 input
