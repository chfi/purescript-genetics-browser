module Genetics.Browser.Biodalliance
       ( initBD
       , BrowserConstructor
       , RenderWrapper
       , sources
       , RendererInfo
       , renderers
       -- , maxHeight
       , addFeatureListener
       , addInitListener
       , setLocation
       , scrollView
       , module Export
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn4, runEffFn2, runEffFn4)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut.Core (JObject)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Options (Option, Options, opt, options)
import Genetics.Browser.Config.Track (BDTrackConfig)
import Genetics.Browser.Types (BD, Biodalliance) as Export
import Genetics.Browser.Types (BD, Biodalliance, Renderer)
import Genetics.Browser.Units (class HCoordinate, Bp, Chr, bp)


foreign import data BrowserConstructor :: Type
foreign import data RenderWrapper :: Type


foreign import initBDimpl :: ∀ eff.
                             Fn3
                             Foreign
                             RenderWrapper
                             BrowserConstructor
                             (HTMLElement -> Eff (bd :: BD | eff) Biodalliance)
-- foreign import initBDimpl :: ∀ eff.
--                              Foreign
--                           -> RenderWrapper
--                           -> BrowserConstructor
--                           -> (HTMLElement -> Eff (bd :: BD | eff) Biodalliance)


-- | Helper function to create a Biodalliance browser instance.
-- | `opts` should contain the BD track configurations (using the `sources` option),
-- | and an optional set of `renderers`
-- | the `RenderWrapper` is exported by Biodalliance as WrappedRenderer.wrapRenderer,
-- | the `BrowserConstructor` is the `Browser` constructor exported by Biodalliance.
-- | Returns a function that takes an element to place the BD instance in,
-- | and places the browser in said element, returning a reference to the instance.

initBD :: ∀ eff.
          Options Biodalliance
       -> RenderWrapper
       -> BrowserConstructor
       -> (HTMLElement -> Eff (bd :: BD | eff) Biodalliance)
initBD opts = runFn3 initBDimpl $ options opts


sources :: Option Biodalliance (Array BDTrackConfig)
sources = opt "sources"

-- Renderers need some extra info for WrappedRenderer to be able to do its thing
type RendererInfo = { name :: String, renderer :: Renderer, canvasHeight :: Number }

renderers :: Option Biodalliance (Array RendererInfo)
renderers = opt "renderers"

maxHeight :: Option Biodalliance Int
maxHeight = opt "maxHeight"



-- | Add a callback that's run when the user clicks on a feature in the BD browser.
-- | The callback receives the clicked on feature.
foreign import addFeatureListenerImpl :: ∀ eff a.
                                         EffFn2 (bd :: BD | eff)
                                         Biodalliance
                                         (JObject -> Eff eff a)
                                         Unit

addFeatureListener :: _
addFeatureListener = runEffFn2 addFeatureListenerImpl

-- | Add a callback that's run when the browser is initialized.
foreign import addInitListener :: forall eff a.
                                  EffFn2 (bd :: BD | eff)
                                  Biodalliance
                                  (Eff (bd :: BD | eff) a)
                                  Unit

onInit :: forall eff a. Biodalliance -> Eff (bd :: BD | eff) a -> Eff (bd :: BD | eff) Unit
onInit bd cb = runEffFn2 addInitListener bd cb

foreign import setLocationImpl :: ∀ eff.
                                  EffFn4 (bd :: BD | eff)
                                  Biodalliance
                                  Chr Bp Bp
                                  Unit


-- | Set the BD viewport to a given chromosome, left-hand edge, and right-hand edge.
setLocation :: ∀ c eff.
               HCoordinate c
            => Biodalliance
            -> Chr -> c -> c
            -> Eff (bd :: BD | eff) Unit
setLocation bd chr xl xr = runEffFn4 setLocationImpl bd chr (bp xl) (bp xr)

foreign import scrollViewImpl :: ∀ eff.
                                 EffFn2 (bd :: BD | eff)
                                 Biodalliance
                                 Bp
                                 Unit

-- | Scroll the BD viewport in the current chromosome by some distance.
scrollView :: ∀ c eff.
              HCoordinate c
           => Biodalliance
           -> c
           -> Eff (bd :: BD | eff) Unit
scrollView bd c = runEffFn2 scrollViewImpl bd (bp c)
