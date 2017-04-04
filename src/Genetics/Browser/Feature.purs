module Genetics.Browser.Feature
       ( Feature
       , ScreenFeature
       , featureToScreen
       , feature
       , unwrapFeature
       , withFeature
       , withFeature'
       , translateFeature
       , scaleFeature
       ) where

import Prelude
import Genetics.Browser.Units (class HCoordinate, toScreen)
import Data.Newtype (class Newtype, wrap)


type FeatureRecord c r = { chr :: String
                         , min :: c
                         , max :: c
                         | r
                         }

newtype Feature c r = Feature (FeatureRecord c r)

type ScreenFeature r = Feature Number r


feature :: ∀ c r. HCoordinate c => FeatureRecord c r -> Feature c r
feature = Feature

unwrapFeature :: ∀ c r. HCoordinate c => Feature c r -> FeatureRecord c r
unwrapFeature (Feature f) = f

withFeature :: ∀ a c r. (FeatureRecord c r -> a) -> Feature c r -> a
withFeature g (Feature f) = g f

withFeature' :: ∀ a c r. Feature c r -> (FeatureRecord c r -> a)  -> a
withFeature' = flip withFeature

featureToScreen :: ∀ c r. HCoordinate c => c -> c -> Feature c r -> ScreenFeature r
featureToScreen offset scale (Feature f) =
  Feature $ f { min = toScreen offset scale f.min
              , max = toScreen offset scale f.max
              }

translateFeature :: ∀ c r. (Newtype c Number, HCoordinate c) => Number -> Feature c r -> Feature c r
translateFeature x (Feature f) = Feature $ f { min = f.min - wrap x
                                             , max = f.max - wrap x
                                             }

scaleFeature :: ∀ c r. (Newtype c Number, HCoordinate c) => Number -> Feature c r -> Feature c r
scaleFeature x (Feature f) = Feature $ f { min = f.min * wrap x
                                         , max = f.max * wrap x
                                         }
