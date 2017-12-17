module Test.Coordinates where

import Genetics.Browser.Types
import Genetics.Browser.Types.Coordinates
import Prelude

import Data.BigInt as BigInt
import Data.Pair (Pair(..))
import Data.Ratio ((%))
import Test.QuickCheck (Result, withHelp, (/==), (===))
import Test.QuickCheck.Gen (Gen, chooseInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.QuickCheck (quickCheck)


browserL :: Int
browserL = 0
browserR :: Int
browserR = 100000


genInterval :: Int -> Int -> Gen (Interval Int)
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


g2i_iso :: Boolean -> Gen Result
g2i_iso genInInterval = do
  let lhs = browserL
      rhs = browserR
  iv'@(Pair l r) <- genInterval lhs rhs

  p' <- if genInInterval
          then chooseInt l r
          else chooseInt lhs rhs

  let p = BigInt.fromInt p'
      iv = BigInt.fromInt <$> iv'
  let px = intervalToGlobal' iv $ globalToInterval' iv p

  pure $ withHelp (iso_prop' (globalToInterval' iv) (intervalToGlobal' iv) p)
           $ "global <-> interval isomorphism failed in " <> show iv' <> ", point " <> show p' <> ", " <> show px



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
            when (intervalsOverlap x y /= t)
              $ fail (show x <> ", " <> show y <> " should " <> h' t <> "overlap")


      -- TODO quickcheck this
      helper p0 p1 true
      helper p1 p2 true
      helper p0 p2 false
      helper p2 p3 true
      helper p1 p4 false

    it "can see if one overlaps with one of many" do
      let f = coveringIntervals


      -- TODO quickcheck this
      f p1 ps `shouldEqual` [p0, p1, p2]
      f p2 ps `shouldEqual` [p1, p2, p3]
      f p3 ps `shouldEqual` [p2, p3]
      f p4 ps `shouldEqual` [p4]


    it "Mapping between global coordinates and local coordinates in a given interval is an isomorphism" do

      quickCheck g2i_iso


    it "Is possible to map from canvas coordinates to local and global coordinates" do
      -- TODO MORE HERE! quickcheck, make sure it's actually good, etc.

      let w = {width: 856.0}
          x = 300.0
      let ratio = BigInt.fromInt 75 % BigInt.fromInt 214
      canvasToView w x `shouldEqual` ratio

      -- sanity check canvasTobrowserpoint
      let v = map (BPoint <<< BigInt.fromInt)  $ Pair 1000 5000
          v' = map (BPoint <<< BigInt.fromInt) $ Pair 500 1500
          p = BPoint $ BigInt.fromInt 2000

      browserPointToCanvas w v p `shouldEqual` 214.0

      let {width,offset} = intervalToScreen w v v'

      width `shouldEqual` 214.0
      offset `shouldEqual` (-107.0)


    it "shiftIntervalBy" do

      -- TODO quickcheck this
      let p' = Pair 1 100000
      shiftIntervalBy' p' (1%2) `shouldEqual` Pair 50000 149999
      shiftIntervalBy' p' (1%99) `shouldEqual` Pair 1011 101010
