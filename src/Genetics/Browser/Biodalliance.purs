module Genetics.Browser.Biodalliance where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Genetics.Browser.Cytoscape (cyFilterByString, elData, evtTarget, onClick)
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
foreign import addFeatureListener :: ∀ cbEff eff.
                                     Biodalliance
                                  -> (BDFeature -> Eff cbEff Unit)
                                  -> Eff (bd :: BD | eff) Unit


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


-- TODO: pretty bad, assumes Bp, etc.
foreign import parseBDFeaturePos :: BDFeature -> { chr :: String
                                                 , min :: Bp
                                                 , max :: Bp
                                                 }


feature' { chr, min, max} = Feature chr min max unit


-- addCyFilter :: ∀ eff. Biodalliance -> Cytoscape -> Eff (bd :: BD | eff) Unit
addCyFilter :: _
addCyFilter bd cy = addFeatureListener bd callback
  where callback :: _
        callback = (cyFilterByString cy) <<< (_.chr <<< parseBDFeaturePos)


addCyCallback :: _
addCyCallback bd cy = onClick cy $ \evt -> case elData (evtTarget evt) of
        Nothing -> pure unit
        Just { chr, pos } -> setLocation bd chr ((bp pos) - (Bp 1000000.0)) ((bp pos) + (Bp 1000000.0))
