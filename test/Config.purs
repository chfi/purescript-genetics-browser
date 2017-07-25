module Test.Config where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), isLeft, isRight)
import Data.Foldable (sequence_)
import Data.Foreign (Foreign, renderForeignError)
import Genetics.Browser.Config (BrowserConfig(..), parseBrowserConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

foreign import validConfig :: Foreign
foreign import validNoBD :: Foreign
foreign import badConfigWrapRenderer :: Foreign
foreign import badConfigBrowser :: Foreign
foreign import badConfigTracks1 :: Foreign
foreign import badConfigTracks2 :: Foreign

parse f = runExcept $ parseBrowserConfig f

spec :: Spec _ Unit
spec = do
  describe "Browser Configuration validation" do
    it "can correctly parse a valid config" do
      isRight (parse validConfig) `shouldEqual` true

    it "fails on configs where `wrapRenderer` is not Function" do
      isLeft (parse badConfigWrapRenderer) `shouldEqual` true

    it "fails on configs where `browser` is not Function" do
      isLeft (parse badConfigBrowser) `shouldEqual` true

    it "fails on configs where `tracks` is not an object with valid keys" do
      isLeft (parse badConfigTracks1) `shouldEqual` true

    it "fails on configs where `tracks` is empty" do
      isLeft (parse badConfigTracks2) `shouldEqual` true

    it "succeeds without wrapRenderer and browser when no BD tracks" do
      case parse validNoBD of
        Left e -> fail "parsing config failed"
        Right (BrowserConfig conf) -> do
          -- conf.wrapRenderer `shouldEqual` Nothing
          -- conf.browser `shouldEqual` Nothing
          fail "not implemented"
