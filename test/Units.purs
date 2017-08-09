module Test.Units where
       -- ( spec
       -- ) where

import Control.Monad.Eff.Class (liftEff)
import Genetics.Browser.Units (class HCoordinate, Bp(..), MBp(..), bp, mbp)
import Prelude
import Test.QuickCheck (quickCheck')
import Test.Spec (Spec, describe, it)

-- spec :: Spec _ Unit
-- spec = do
--   let isomorphism :: ∀ c. (HCoordinate c) => c -> Boolean
--       isomorphism x = bp x  - bp (mbp x) < Bp 0.000001 &&
--                       mbp x - mbp (bp x) < MBp 0.000001
--   describe "Units and coordinates" do
--     it "Bp and MBp are isomorphic" $ do
--       liftEff $ quickCheck' 1000 (isomorphism :: Bp -> Boolean)
--       liftEff $ quickCheck' 1000 (isomorphism :: MBp -> Boolean)
