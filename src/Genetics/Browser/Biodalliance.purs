module Genetics.Browser.Biodalliance where

import Prelude
import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut.Core (JObject)
import Data.Foreign (Foreign)
import Data.Options (Option, Options(..), opt, options)
import Genetics.Browser.Config.Track (BDTrackConfig(..))
import Genetics.Browser.Types (BD, Biodalliance, Renderer(..))
import Genetics.Browser.Units (class HCoordinate, Bp, Chr(..), bp)
import Unsafe.Coerce (unsafeCoerce)

foreign import data BrowserConstructor :: Type
foreign import data RenderWrapper :: Type

newtype BDOptions = BDOptions Foreign

foreign import initBDimpl :: ∀ eff.
                             BDOptions
                          -> RenderWrapper
                          -> BrowserConstructor
                          -> (HTMLElement -> Eff eff Biodalliance)

initBD :: ∀ eff.
          Options Biodalliance
       -> RenderWrapper
       -> BrowserConstructor
       -> (HTMLElement -> Eff eff Biodalliance)
initBD opts = initBDimpl (BDOptions $ options opts)


sources :: Option Biodalliance (Array BDTrackConfig)
sources = opt "sources"

-- Renderers need some extra info for WrappedRenderer to be able to do its thing
type RendererInfo = { name :: String, renderer :: Renderer, canvasHeight :: Number }

renderers :: Option Biodalliance (Array RendererInfo)
renderers = opt "renderers"


-- newtype SubConfig = SubConfig { multi_id :: String, offset :: Number }

-- type GWASConfig = { name :: String
--                   , forceReduction :: Int
--                   , bwgUri :: String
--                   , renderer :: String
--                   , sub :: SubConfig
--                   }

-- gwasTrack :: GWASConfig -> BDTrack
-- gwasTrack = unsafeCoerce

-- gwasConfig :: GWASConfig
-- gwasConfig = { name: "GWAS"
--              , renderer: "gwasRenderer"
--              , sub: SubConfig { multi_id: "multi_1"
--                               , offset: 0.0
--                               }
--              , forceReduction: -1
--              , bwgUri: "http://localhost:8080/gwascatalog.bb"
--              }

-- genomeTrack :: String -> String -> BDTrack
-- genomeTrack name uri =

-- gwasTrack :: String -> String -> String -> BDTrack
-- gwasTrack name renderer


-- TODO: should probably be a bit safer than just sending a JObject. future problem tho~~
-- TODO: Should also handle potential parents of objects, but especially which track was clicked
--         -- the latter will be relevant to native PS tracks, probably
foreign import addFeatureListener :: ∀ eff a.
                                     Biodalliance
                                  -> (JObject -> Eff (bd :: BD | eff) a)
                                  -> Eff (bd :: BD | eff) Unit

foreign import addInitListener :: forall eff a.
                                  Biodalliance
                               -> Eff (bd :: BD | eff) a
                               -> Eff (bd :: BD | eff) Unit

onInit :: forall eff a. Biodalliance -> Eff (bd :: BD | eff) a -> Eff (bd :: BD | eff) Unit
onInit bd cb = addInitListener bd cb


foreign import setLocationImpl :: ∀ eff.
                                  Biodalliance
                               -> Chr -> Bp -> Bp
                               -> Eff (bd :: BD | eff) Unit

setLocation :: ∀ c eff. HCoordinate c =>
               Biodalliance
            -> Chr -> c -> c
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
