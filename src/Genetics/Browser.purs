module Genetics.Browser
       where

import Prelude
import Genetics.Browser.Biodalliance as Biodalliance
import Genetics.Browser.Cytoscape as Cytoscape
import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot as QTL
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Const (Const(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (type (<\/>), Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Source.QTL (fetch)
import Genetics.Browser.Types (BD, Biodalliance, CY, Cytoscape, Renderer)
import Genetics.Browser.Units (Bp(..))
import Global.Unsafe (unsafeStringify)
import Halogen.VDom.Driver (runUI)

import Genetics.Browser.UI.Container as Container

-- TODO: ugly that the LinePlotConfig is referred to at all outside Lineplot.purs...
qtlGlyphify :: LinePlotConfig -> Renderer
qtlGlyphify = QTL.render

gwasGlyphify :: Renderer
gwasGlyphify = GWAS.render


qtlFetch = fetch


cytoscape = Cytoscape.cytoscape
setOn = Cytoscape.setOn
-- setBDOn = Cytoscape.setBDOn

ajaxCytoscape = Cytoscape.ajaxCytoscape
ajaxAddEles = Cytoscape.ajaxAddEles
filterElements = Cytoscape.filterElements
elesOn = Cytoscape.elesOn
cyFilter = Cytoscape.cyFilter
cyReset = Cytoscape.resetFilter

addCyFilter = Biodalliance.addCyFilter
addBdScrollCallback = Biodalliance.addCyCallback


-- main :: Eff (HA.HalogenEffects ()) Unit
-- TODO: creating BD should be in a promise or something.
main :: Biodalliance -> Eff _ Unit
main bd = HA.runHalogenAff do
  liftEff $ log "running main"
  -- _ <- HA.awaitLoad
  el <- HA.selectElement (wrap "#psgbHolder")
  case el of
    Nothing -> do
      liftEff $ log "no element for browser!"
    Just el' -> do
      io <- runUI Container.component unit el'
      liftEff $ log "attaching BD"
      io.query $ H.action (Container.SetBD bd)
      liftEff $ log "attached!"
      liftEff $ log "moving to chr 3"
      io.query $ H.action (Container.BDJump "Chr3" 1000000.0 10000000.0)
