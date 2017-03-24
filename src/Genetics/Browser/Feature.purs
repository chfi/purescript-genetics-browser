module Genetics.Browser.Feature
       ( Feature(..)
       , translateFeature
       , scaleFeature
       , chrToScreen
       ) where

import Prelude
import Data.Newtype (class Newtype)
import Genetics.Browser.Types (View)


newtype Feature r = Feature { min :: Number
                            , max :: Number
                            | r
                            }

derive instance newtypeFeature :: Newtype (Feature r) _

translateFeature :: ∀ r. Number -> Feature r -> Feature r
translateFeature x (Feature f) = Feature $ f { min = f.min - x
                                             , max = f.max - x
                                             }

scaleFeature :: ∀ r. Number -> Feature r -> Feature r
scaleFeature x (Feature f) = Feature $ f { min = f.min * x
                                         , max = f.max * x
                                         }


chrToScreen :: ∀ r. View -> Feature r -> Feature r
chrToScreen v = scaleFeature v.scale <<< translateFeature v.viewStart
