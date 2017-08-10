module Test.Units
       ( prop_isomorphism
       ) where

import Prelude

import Data.Int (toNumber)
import Genetics.Browser.Units (class HCoordinate, Bp(..), MBp(..), bp, mbp)
import Jack (Gen, Property, chooseInt, forAllRender, property)


genFloat :: Int -> Int -> Gen Number
genFloat a b = toNumber <$> chooseInt a b

genBp :: Gen Bp
genBp = Bp <$> genFloat 1 10000000

genMBp :: Gen MBp
genMBp = MBp <<< ((*) 0.0000001) <$> genFloat 1 10000000

genBoth :: Gen {bp' :: Bp, mbp' :: MBp}
genBoth = do
  bp' <- genBp
  mbp' <- genMBp
  pure $ { bp', mbp' }


isomorphism :: ∀ c. (HCoordinate c) => c -> Boolean
isomorphism x = bp x  - bp (mbp x) < Bp 0.000001 &&
                mbp x - mbp (bp x) < MBp 0.000001

prop_isomorphism :: Property
prop_isomorphism =
  forAllRender (\ {bp', mbp'} -> show bp' <> ", " <> show mbp') genBoth \{bp', mbp'} ->
    property $ isomorphism bp' && isomorphism mbp'

-- spec :: Spec _ Unit
-- spec = do
--   let isomorphism :: ∀ c. (HCoordinate c) => c -> Boolean
--       isomorphism x = bp x  - bp (mbp x) < Bp 0.000001 &&
--                       mbp x - mbp (bp x) < MBp 0.000001
--   describe "Units and coordinates" do
--     it "Bp and MBp are isomorphic" $ do
--       liftEff $ quickCheck' 1000 (isomorphism :: Bp -> Boolean)
--       liftEff $ quickCheck' 1000 (isomorphism :: MBp -> Boolean)
