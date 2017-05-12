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
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Source.QTL (fetch)
import Genetics.Browser.Types (BD, Biodalliance, CY, Cytoscape, Renderer)
import Genetics.Browser.Units (Bp(..))
import Global.Unsafe (unsafeStringify)
import Halogen.VDom.Driver (runUI)

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

type State = Unit

data Query a
  = Nop a
  | SetBD Biodalliance a
  | BDScroll Number a
  | BDJump String Number Number a

type BDState = { bd :: Maybe Biodalliance }

data BDQuery a
  = Scroll Number a
  | Jump String Number Number a
  | Initialize Biodalliance a

type BDEffects eff = (bd :: BD | eff)

bdComponent :: ∀ eff. H.Component HH.HTML BDQuery Unit Void (Aff (BDEffects eff))
bdComponent =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: BDState
  initialState = { bd: Nothing }

  -- doesn't actually render anything...
  render :: BDState -> H.ComponentHTML BDQuery
  render = const $ HH.div [ HP.ref (H.RefLabel "bd") ] []

  eval :: BDQuery ~> H.ComponentDSL BDState BDQuery Void (Aff (BDEffects eff))
  -- eval :: AceQuery ~> H.ComponentDSL AceState AceQuery AceOutput (Aff (AceEffects eff))
  eval = case _ of
    Initialize bd next -> do
      H.modify (_ { bd = Just bd })
      pure next
    Scroll n next -> do
      mbd <- H.gets _.bd
      case mbd of
        Nothing -> pure next
        Just bd -> do
          liftEff $ Biodalliance.scrollView bd (Bp n)
          pure next
      -- pure next
    Jump chr xl xr next -> do
      mbd <- H.gets _.bd
      case mbd of
        Nothing -> pure next
        Just bd -> do
          liftEff $ Biodalliance.setLocation bd chr (Bp xl) (Bp xr)
          pure next


data BDSlot = BDSlot
derive instance eqBDSlot :: Eq BDSlot
derive instance ordBDSlot :: Ord BDSlot
component :: ∀ eff. H.Component HH.HTML Query Unit Void (Aff (BDEffects eff))
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

  render :: State -> H.ParentHTML Query BDQuery BDSlot (Aff (BDEffects eff))
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
      , HH.slot BDSlot bdComponent unit absurd
      ]

  eval :: Query ~> H.ParentDSL State Query BDQuery BDSlot Void (Aff (BDEffects eff))
  eval = case _ of
    Nop next -> do
      pure next
    SetBD bd next -> do
      _ <- H.query BDSlot $ H.action (Initialize bd)
      pure next
    BDScroll dist next -> do
      _ <- H.query BDSlot $ H.action (Scroll dist)
      pure next
    BDJump chr xl xr next -> do
      _ <- H.query BDSlot $ H.action (Jump chr xl xr)
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
      io.query $ H.action (BDJump "Chr3" 1000000.0 10000000.0)
