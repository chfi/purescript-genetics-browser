module Genetics.Browser.Feature
       ( Feature(..)
       , feature
       , ScreenFeature
       , featureToScreen
       , translateFeature
       , scaleFeature
       ) where

import Prelude
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Foreign (Foreign)
import Genetics.Browser.Units (class HCoordinate, toScreen)
import Test.QuickCheck (class Arbitrary, arbitrary)

-- String = chromosome, c = coordinate, r = rest of feature data
data Feature c r = Feature String c c r

feature :: ∀ c r. HCoordinate c => String -> c -> c -> r -> Feature c r
feature = Feature

-- don't want to export the Feature constructor,
-- since it should only be created with a HCoordinate
-- unwrapFeature :: ∀ c r. Feature c r -> { chr :: String, xl :: c, xr :: c, rest :: r }
-- unwrapFeature (Feature chr xl xr r) = { chr, xl, xr, rest: r }


instance arbitraryFeature :: (Arbitrary c, Arbitrary r) => Arbitrary (Feature c r) where
  arbitrary = do
    chr <- arbitrary
    xl <- arbitrary
    xr <- arbitrary
    r <- arbitrary
    pure $ Feature chr xl xr r


instance bifunctorFeature :: Bifunctor Feature where
  bimap f g (Feature chr xl xr r) = Feature chr (f xl) (f xr) (g r)


-- represents features whose horizontal position is on the canvas
type ScreenFeature r = Feature Number r

newtype ForeignFeature = ForeignFeature Foreign


featureToScreen :: ∀ c. HCoordinate c => c -> c -> Feature c ~> ScreenFeature
featureToScreen offset scale = lmap (toScreen offset scale)

translateFeature :: ∀ c. HCoordinate c => c -> Feature c ~> Feature c
translateFeature x = lmap (_ - x)

scaleFeature :: ∀ c. HCoordinate c => c -> Feature c ~> Feature c
scaleFeature x = lmap (_ * x)
