module Genetics.Browser.UI.Container
       where

import Prelude
import Data.StrMap as StrMap
import Genetics.Browser.Biodalliance as Biodalliance
import Genetics.Browser.Cytoscape as Cytoscape
import Genetics.Browser.Events as GBE
import Genetics.Browser.Feature.Foreign as FF
import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot as QTL
import Genetics.Browser.UI.Biodalliance as UIBD
import Genetics.Browser.UI.Cytoscape as UICy
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Coroutine (Consumer, consumer)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut.Core (JObject)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Foreign (Foreign, readString)
import Data.Functor.Coproduct.Nested (type (<\/>), Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.StrMap (StrMap)
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Source.QTL (fetch)
import Genetics.Browser.Types (BD, Biodalliance, CY, Cytoscape, Renderer)
import Genetics.Browser.Units (Bp(..))
import Global.Unsafe (unsafeStringify)
import Halogen.VDom.Driver (runUI)
import Unsafe.Coerce (unsafeCoerce)

qtlGlyphify :: LinePlotConfig -> Renderer
qtlGlyphify = QTL.render

gwasGlyphify :: Renderer
gwasGlyphify = GWAS.render


data Track = BDTrack | CyTrack

type State = Unit

data Query a
  = Nop a
  | CreateBD (forall eff. HTMLElement -> Eff eff Biodalliance) a
  | BDScroll Bp a
  | BDJump String Bp Bp a
  | CreateCy String a
  | ResetCy a
  | CyClicked Cytoscape.ParsedEvent a
  | BDClicked JObject a
  | DistEvent Track GBE.Event a

type ChildSlot = Either2 UIBD.Slot UICy.Slot

type ChildQuery = UIBD.Query <\/> UICy.Query <\/> Const Void

type Effects eff = UIBD.Effects (UICy.Effects eff)

component :: ∀ eff. H.Component HH.HTML Query Unit Void (Aff (Effects eff))
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


            -- #svgHolder {
            --     width: 100%,
            --     float: left;
            -- }
            -- #cyHolder {
            --     width: 100%;
            --     height: 300px;
            --     float: left;
            --     display: block;
            -- }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (Effects eff))
  render state =
    HH.div_
      [ HH.div_
        [ HH.button
          [  HE.onClick (HE.input_ (BDScroll (Bp (-1000000.0))))
          ]
          [ HH.text "Scroll left 1MBp" ]
        , HH.button
          [  HE.onClick (HE.input_ (BDScroll (Bp 1000000.0)))
          ]
          [ HH.text "Scroll right 1MBp" ]
        , HH.button
          [  HE.onClick (HE.input_ ResetCy)
          ]
          [ HH.text "Reset cytoscape" ]
          -- these divs are used to control the sizes of the subcomponents without having to query the children
        , HH.div
            [] [HH.slot' CP.cp1 UIBD.Slot UIBD.component unit handleBDMessage]
        , HH.div
            [] [HH.slot' CP.cp2 UICy.Slot UICy.component unit handleCyMessage]
        ]
      -- , HH.div
      --     [
      --       -- fix stylesheet - should be on the right hand side, not too wide...
      --     ]
      --     [
      --     ]
      ]


  handleBDMessage :: UIBD.Message -> Maybe (Query Unit)
  handleBDMessage UIBD.Initialized = Just $ ResetCy unit
  handleBDMessage (UIBD.Clicked obj) = Just $ BDClicked obj unit

     -- TODO the event source track should be handled automatically somehow
  handleCyMessage :: UICy.Output -> Maybe (Query Unit)
  handleCyMessage (UICy.SendEvent ev) = Just $ DistEvent CyTrack ev unit

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (Effects eff))
  eval = case _ of
    Nop next -> do
      pure next
    CreateBD bd next -> do
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
    ResetCy next -> do
      _ <- H.query' CP.cp2 UICy.Slot $ H.action UICy.Reset
      pure next
    CyClicked (Cytoscape.ParsedEvent ev) next -> do

      case ev.target of
        Left el -> do
          d <- liftEff $ Cytoscape.eleGetAllData el
          case UICy.getCyLocation d of
            Left err  -> liftEff $ log err
            Right loc -> do
              liftEff $ log $ "cy chr: " <> loc.chr
              _ <- H.query' CP.cp1 UIBD.Slot $ H.action
                     (UIBD.Jump loc.chr
                      (loc.pos * Bp 1000000.0 - Bp 5000000.0)
                      (loc.pos * Bp 1000000.0 + Bp 5000000.0))
              pure unit
          pure unit

        Right cy -> liftEff $ log "Clicked outside element"
      pure next

    BDClicked obj next -> do
      case getBDRange obj of
        Left err -> liftEff $ log err
        Right r  -> do
          liftEff $ log $ "bd chr: " <> r.chr

          let f el = case UICy.getCyLocation el of
                Left _-> false
                Right loc -> loc.chr == "Chr12"
          _ <- H.query' CP.cp2 UICy.Slot $ H.action (UICy.Filter f)
          pure unit
      pure next

    DistEvent from ev next -> do

      case from of
        BDTrack ->
          H.query' CP.cp2 UICy.Slot $ H.action (UICy.RecvEvent ev)
        CyTrack ->
          H.query' CP.cp1 UIBD.Slot $ H.action (UIBD.RecvEvent ev)

      pure next





getBDRange :: JObject -> Either String { chr :: String, xl :: Bp, xr :: Bp }
getBDRange = FF.parseFeatureRange { chrKey: "chr"
                                  , xlKey: "min"
                                  , xrKey: "max"
                                  }




main :: (forall eff. HTMLElement -> Eff eff Biodalliance) -> Eff _ Unit
main mkBd = HA.runHalogenAff do
  liftEff $ log "running main"
  HA.awaitLoad
  el <- HA.selectElement (wrap "#psgbHolder")
  case el of
    Nothing -> do
      liftEff $ log "no element for browser!"
    Just el' -> do
      io <- runUI component unit el'
      liftEff $ log "creating BD"
      io.query $ H.action (CreateBD mkBd)
      liftEff $ log "created BD!"
      liftEff $ log "creating Cy.js"
      io.query $ H.action (CreateCy "http://localhost:8080/eles.json")
      liftEff $ log "created cy!"
