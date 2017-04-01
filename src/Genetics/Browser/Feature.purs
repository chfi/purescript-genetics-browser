module Genetics.Browser.Feature
       ( Feature
       , feature
       , unwrapFeature
       , withFeature
       , withFeature'
       , translateFeature
       , scaleFeature
       -- , chrToScreen
       ) where

import Prelude

import Genetics.Browser.Units
import Data.Newtype (class Newtype, over, under, unwrap, wrap)
import Genetics.Browser.Types (View)


type FeatureRecord c r = { chr :: String
                         , min :: c
                         , max :: c
                         | r
                         }

newtype Feature c r = Feature (FeatureRecord c r)


feature :: ∀ c r. HCoordinate c => FeatureRecord c r -> Feature c r
feature = Feature

unwrapFeature :: ∀ c r. HCoordinate c => Feature c r -> FeatureRecord c r
unwrapFeature (Feature f) = f

withFeature :: ∀ a c r. (FeatureRecord c r -> a) -> Feature c r -> a
withFeature g (Feature f) = g f

withFeature' :: ∀ a c r. Feature c r -> (FeatureRecord c r -> a)  -> a
withFeature' = flip withFeature


-- derive instance newtypeFeature :: Newtype (Feature c r) _

-- instance newtypeFeature :: Newtype (Feature r) r where
--   wrap {chr, min, max} = Feature {chr, min, max}
--   unwrap (Feature f) = f

translateFeature :: ∀ c r. (Newtype c Number, HCoordinate c) => Number -> Feature c r -> Feature c r
translateFeature x (Feature f) = Feature $ f { min = f.min - wrap x
                                             , max = f.max - wrap x
                                             }

-- scaleFeature :: ∀ r. Number -> Feature r -> Feature r
scaleFeature :: ∀ c r. (Newtype c Number, HCoordinate c) => Number -> Feature c r -> Feature c r
scaleFeature x (Feature f) = Feature $ f { min = f.min * wrap x
                                         , max = f.max * wrap x
                                         }


-- chrToScreen :: ∀ c r. (Newtype c Number, HCoordinate c) => View -> Feature c r -> Feature c r
-- chrToScreen v = scaleFeature v.scale <<< translateFeature v.viewStart
