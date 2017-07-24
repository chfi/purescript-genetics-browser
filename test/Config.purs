module Test.Config where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (isLeft, isRight)
import Data.Foreign (Foreign)
import Genetics.Browser.Config (parseBrowserConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

foreign import validConfig :: Foreign
foreign import badConfigWrapRenderer :: Foreign
foreign import badConfigBrowser :: Foreign
foreign import badConfigTracks :: Foreign

parse f = runExcept $ parseBrowserConfig f

specConfig :: ∀ eff. Spec _ Unit
specConfig = do
  describe "Configuration validation" do
    it "can correctly parse a valid config" do
      isRight (runExcept $ parseBrowserConfig validConfig) `shouldEqual` true
    it "fails on configs where `wrapRenderer` is not Function" do
      isLeft (parse badConfigWrapRenderer) `shouldEqual` true
    it "fails on configs where `browser` is not Function" do
      isLeft (parse badConfigBrowser) `shouldEqual` true
    it "fails on configs where `tracks` is not an object with valid keys" do
      isLeft (parse badConfigTracks) `shouldEqual` true
