module Genetics.Browser.UI.Container
       where

import Prelude
import Genetics.Browser.Biodalliance as Biodalliance
import Genetics.Browser.Cytoscape as Cytoscape
import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot as QTL
import Genetics.Browser.UI.Biodalliance as UIBD
import Genetics.Browser.UI.Cytoscape as UICy
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
import Halogen.Component.ChildPath as CP
import Halogen.VDom.Driver (runUI)

qtlGlyphify :: LinePlotConfig -> Renderer
qtlGlyphify = QTL.render

gwasGlyphify :: Renderer
gwasGlyphify = GWAS.render

cytoscape = Cytoscape.cytoscape
setOn = Cytoscape.setOn

ajaxCytoscape = Cytoscape.ajaxCytoscape
ajaxAddEles = Cytoscape.ajaxAddEles
filterElements = Cytoscape.filterElements
elesOn = Cytoscape.elesOn
cyFilter = Cytoscape.cyFilter
cyReset = Cytoscape.resetFilter



type State = Unit

data Query a
  = Nop a
  | SetBD Biodalliance a
  | BDScroll Number a
  | BDJump String Number Number a
  | CreateCy String a

type ChildSlot = Either2 UIBD.Slot UICy.Slot

type ChildQuery = UIBD.Query <\/> UICy.Query <\/> Const Void

-- type Effects e = _

component :: ∀ eff. H.Component HH.HTML Query Unit Void (Aff _)
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = unit

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff _)
  render state =
    HH.div_
      [ HH.button
        [  HE.onClick (HE.input_ (BDScroll (-1000000.0)))
        ]
        [ HH.text "Scroll left 1MBp" ]
      , HH.button
        [  HE.onClick (HE.input_ (BDScroll 1000000.0))
        ]
        [ HH.text "Scroll right 1MBp" ]
      , HH.slot' CP.cp1 UIBD.Slot UIBD.component unit absurd
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff _)
  eval = case _ of
    Nop next -> do
      pure next
    SetBD bd next -> do
      _ <- H.query' CP.cp1 UIBD.Slot $ H.action (UIBD.Initialize bd)
      pure next
    BDScroll dist next -> do
      _ <- H.query' CP.cp1 UIBD.Slot $ H.action (UIBD.Scroll dist)
      pure next
    BDJump chr xl xr next -> do
      _ <- H.query' CP.cp1 UIBD.Slot $ H.action (UIBD.Jump chr xl xr)
      pure next
    CreateCy div next -> do
      _ <- H.query' CP.cp2 UICy.Slot $ H.action (UICy.Initialize div)
      pure next


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
      io <- runUI component unit el'
      liftEff $ log "attaching BD"
      io.query $ H.action (SetBD bd)
      liftEff $ log "attached!"
      liftEff $ log "moving to chr 3"
      liftEff $ log ":("
      liftEff $ log "well this is crazy"
      io.query $ H.action (BDJump "Chr3" 1000000.0 10000000.0)
      liftEff $ log "wait it blocks after moving???"
      liftEff $ log "creating Cy.js"
      io.query $ H.action (CreateCy "cyHolder")
      liftEff $ log "created cy!"
