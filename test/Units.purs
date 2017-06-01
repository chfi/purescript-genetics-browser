module Test.Units where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Genetics.Browser.Units (class HCoordinate, Bp(..), MBp(..), bp, mbp)
import Prelude
import Test.QuickCheck (quickCheck')
import Test.Spec (Spec, describe, it)

unitIsoSpec :: ∀ eff. Spec ( console :: CONSOLE
                           , random :: RANDOM
                           , exception :: EXCEPTION
                           | eff
                           ) Unit
unitIsoSpec = do
  let isomorphism :: ∀ c. (HCoordinate c) => c -> Boolean
      isomorphism x = bp x  - bp (mbp x) < Bp 0.000001 &&
                      mbp x - mbp (bp x) < MBp 0.000001
  describe "Units and coordinates" do
    it "Bp and MBp are isomorphic" $ do
      liftEff $ quickCheck' 1000 (isomorphism :: Bp -> Boolean)
      liftEff $ quickCheck' 1000 (isomorphism :: MBp -> Boolean)
