module Genetics.Browser.Biodalliance where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Genetics.Browser.Feature (Feature(..))
import Genetics.Browser.Types (BD, BDFeature, Biodalliance, Cytoscape)
import Genetics.Browser.Units (class HCoordinate, Bp(..), bp)

-- addFeatureListener(callback)

-- Called when the user clicks on a feature. Parameters passed to the callback are:
-- event: the DOM MouseEvent which triggered this callback.
-- feature: the feature which the user clicked on.
-- hit: an array of Feature and Group objects representing the clicked feature plus any parents.
-- tier: the Dalliance tier in which the click occurred.

-- for now we only send the feature
foreign import addFeatureListener :: ∀ eff.
                                     Biodalliance
                                  -> (forall eff2. BDFeature -> Eff eff2 Unit)
                                  -> Eff (bd :: BD | eff) Unit

foreign import addInitListener :: forall eff a.
                                  Biodalliance
                               -> Eff (bd :: BD | eff) a
                               -> Eff (bd :: BD | eff) Unit

onInit :: forall eff a. Biodalliance -> Eff (bd :: BD | eff) a -> Eff (bd :: BD | eff) Unit
onInit bd cb = addInitListener bd cb


foreign import setLocationImpl :: ∀ eff.
                                  Biodalliance
                               -> String -> Bp -> Bp
                               -> Eff (bd :: BD | eff) Unit

setLocation :: ∀ c eff. HCoordinate c =>
               Biodalliance
            -> String -> c -> c
            -> Eff (bd :: BD | eff) Unit
setLocation bd chr xl xr = setLocationImpl bd chr (bp xl) (bp xr)

foreign import scrollViewImpl :: ∀ eff.
                                 Biodalliance
                              -> Bp
                              -> Eff (bd :: BD | eff) Unit

scrollView :: ∀ c eff. HCoordinate c =>
              Biodalliance
           -> c
           -> Eff (bd :: BD | eff) Unit
scrollView bd = scrollViewImpl bd <<< bp
