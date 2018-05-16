module Test.Coordinates where

import Genetics.Browser.Types
import Genetics.Browser.Types.Coordinates
import Prelude

import Data.BigInt as BigInt
import Data.Pair (Pair(..))
import Test.QuickCheck (Result, withHelp, (/==), (===))
import Test.QuickCheck.Gen (Gen, chooseInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.QuickCheck (quickCheck)


browserL :: Int
browserL = 0
browserR :: Int
browserR = 100000


genInterval :: Int -> Int -> Gen (Pair Int)
genInterval l r = do
  x <- chooseInt l r
  y <- chooseInt x r
  pure $ Pair (min x y) (max x y)

iso_prop' :: forall a b.
             Eq a
          => Eq b
          => (a -> b)
          -> (b -> a)
          -> a
          -> Boolean
iso_prop' f g x =
  let y = f x
      x' = g y
      y' = f x'
      x'' = g y'
  in x == x'


spec :: Spec _ Unit
spec = do
  describe "Intervals" do

      -- p0 and p1 overlap
      -- p1 and p2 overlap
      -- p0 and p2 do not overlap

      -- p3 overlaps with p2
      -- p4 does not overlap

    let p0 = Pair 0 0
        p1 = Pair 0 2
        p2 = Pair 2 4
        p3 = Pair 3 4
        p4 = Pair (-2) (-1)

    let ps = [p0, p1, p2, p3, p4]

    it "can see if two overlap" do
      let h' x = if x then "" else "not "
          helper x y t =
            when (pairsOverlap x y /= t)
              $ fail (show x <> ", " <> show y <> " should " <> h' t <> "overlap")


      -- TODO quickcheck this
      helper p0 p1 true
      helper p1 p2 true
      helper p0 p2 false
      helper p2 p3 true
      helper p1 p4 false

    -- it "can see if one overlaps with one of many" do

    -- it "Mapping between global coordinates and local coordinates in a given interval is an isomorphism" do


    -- it "Is possible to map from canvas coordinates to local and global coordinates" do


    it "translate one pair relative to its size" do

      -- TODO quickcheck this
      let p' = BigInt.fromInt <$> Pair 1     100000
          t1 = BigInt.fromInt <$> Pair 50000 149999
          t2 = BigInt.fromInt <$> Pair 99000 198999
      translatePairBy p' 0.50 `shouldEqual` t1
      translatePairBy p' 0.99 `shouldEqual` t2
