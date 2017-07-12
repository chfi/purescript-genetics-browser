module Genetics.Browser.UI.Container
       where

import Prelude
import Control.Monad.Aff.Bus as Bus
import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot as QTL
import Genetics.Browser.UI.Biodalliance as UIBD
import Genetics.Browser.UI.Cytoscape as UICy
import Genetics.Browser.UI.Events.Biodalliance as BDEvents
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Rec.Class (forever)
import DOM.HTML.Types (HTMLElement)
import Data.Array (null)
import Data.Const (Const)
import Data.Either.Nested (Either2, Either1)
import Data.Foldable (foldMap)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Data.Options (Options, (:=))
import Data.Symbol (SProxy(..))
import Data.Tuple (fst)
import Data.Variant (Variant, default, on)
import Genetics.Browser.Biodalliance (RendererInfo, initBD, renderers, sources)
import Genetics.Browser.Config (BrowserConfig(..))
import Genetics.Browser.Config.Track (CyGraphConfig, validateConfigs)
import Genetics.Browser.Events (Event(..), Location, Range)
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Types (BD, Biodalliance, Renderer)
import Genetics.Browser.Units (Bp(Bp), Chr)
import Global.Unsafe (unsafeStringify)
import Halogen.VDom.Driver (runUI)
import Unsafe.Coerce (unsafeCoerce)




setBDHandler :: forall r .
                (Biodalliance -> Variant r -> Eff (bd :: BD, console :: CONSOLE, avar :: AVAR ) Unit)
             -> BusR (Variant r)
             -> Biodalliance
             -> Aff ( bd :: BD, console :: CONSOLE, avar :: AVAR  ) Unit
setBDHandler h bus bd = forever do
  val <- Bus.read bus
  liftEff $ h bd val


bdHandler :: forall eff. Biodalliance -> Variant (location :: Location, range :: Range) -> Eff (console :: CONSOLE | eff) Unit
bdHandler bd =
  default (log "default event got")
  # on (SProxy :: SProxy "location") (\loc -> log "got location")
  # on (SProxy :: SProxy "range") (\ran -> log "got range")




qtlGlyphify :: LinePlotConfig -> Renderer
qtlGlyphify = QTL.render

gwasGlyphify :: Renderer
gwasGlyphify = GWAS.render


data Track = BDTrack | CyTrack

type State = Unit

data Query a
  = Nop a
  | CreateBD (∀ eff. HTMLElement -> Eff (bd :: BD | eff) Biodalliance) a
  -- | AttachBDHandler (forall r eff. BDEventInHandler r eff) a
  -- | AttachBDHandler (forall eff. Biodalliance -> Aff (bd :: BD, console :: CONSOLE, avar :: AVAR | eff) Unit) a
  | AttachBDHandler (forall eff. Biodalliance -> Aff eff Unit) a
  | BDScroll Bp a
  | BDJump Chr Bp Bp a
  | CreateCy String a
  | ResetCy a
  -- | DistributeEvent Track (Event r) a

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

  -- addCyGraph :: Maybe CyGraphConfig -> _
  -- addCyGraph = case _ of
  --   Nothing -> []
  --   Just cy -> [HH.div [] [HH.slot' CP.cp2 UICy.Slot UICy.component unit handleCyMessage]]

  -- handleBDMessage :: UIBD.Message bdm -> Maybe (Query r Unit)
  handleBDMessage :: _
  -- handleBDMessage UIBD.Initialized = Just $ ResetCy unit
  handleBDMessage _ = Nothing
  -- handleBDMessage (UIBD.SendEvent obj) = Just $ DistributeEvent BDTrack obj unit

     -- TODO the event source track should be handled automatically somehow
  handleCyMessage :: _
  handleCyMessage _ = Nothing
  -- handleCyMessage :: UICy.Output cym -> Maybe (Query r Unit)
  -- handleCyMessage (UICy.SendEvent ev) = Just $ DistributeEvent CyTrack ev unit

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Aff (Effects eff))
  eval = case _ of
    Nop next -> do
      pure next

    CreateBD bd next -> do
      _ <- H.query' CP.cp1 UIBD.Slot $ H.action (UIBD.Initialize bd)
      pure next
    AttachBDHandler bd next -> do
      _ <- H.query' CP.cp1 UIBD.Slot $ H.action (UIBD.AttachHandler bd)
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

    -- DistributeEvent from ev next -> do
    --   liftEff $ log "container is distributing event:"
    --   liftEff $ log $ unsafeStringify ev

      -- _ <- case from of
        -- BDTrack ->
        --   H.query' CP.cp2 UICy.Slot $ H.action (UICy.RecvEvent ev)
        -- CyTrack ->
        --   H.query' CP.cp1 UIBD.Slot $ H.action (UIBD.RecvEvent ev)

      -- pure next


qtlRenderer :: RendererInfo
qtlRenderer = { name: "qtlRenderer"
              , renderer: qtlGlyphify { minScore: 4.0
                                      , maxScore: 6.0
                                      , color: "#ff0000"
                                      }
              , canvasHeight: 200.0
              }

gwasRenderer :: RendererInfo
gwasRenderer = { name: "gwasRenderer"
               , renderer: gwasGlyphify
               , canvasHeight: 300.0
               }

bdOpts :: Options Biodalliance
bdOpts = renderers := [ qtlRenderer, gwasRenderer ]


main :: BrowserConfig -> Eff _ Unit
main (BrowserConfig { wrapRenderer, browser, tracks }) = HA.runHalogenAff do

  let {bdTracks, cyGraphs} = validateConfigs tracks

      opts' = bdOpts <> sources := bdTracks.results

  liftEff $ log $ "BDTrack errors: " <> foldMap ((<>) ", ") bdTracks.errors
  liftEff $ log $ "CyGraph errors: " <> foldMap ((<>) ", ") cyGraphs.errors

  let mkBd :: (∀ eff. HTMLElement -> Eff (bd :: BD | eff) Biodalliance)
      mkBd = initBD opts' wrapRenderer browser

  liftEff $ log "running main"
  HA.awaitLoad
  el <- HA.selectElement (wrap "#psgbHolder")
  case el of
    Nothing -> do
      liftEff $ log "no element for browser!"
    Just el' -> do
      io <- runUI component unit el'
      liftEff $ log "creating BD event in bus"
      busTuple <- Bus.split <$> Bus.make
      let h :: Biodalliance -> Aff ( bd :: BD, console :: CONSOLE , avar :: AVAR) Unit
          h = setBDHandler bdHandler (fst busTuple)
      liftEff $ log "creating BD"
      io.query $ H.action (CreateBD mkBd)
      -- io.query $ H.action (AttachBDHandler h)
      io.query $ H.action (AttachBDHandler (unsafeCoerce h))
      liftEff $ log "created BD!"
      liftEff $ log $ "cytoscape enabled: " <> show (not null cyGraphs.results)
      -- when (not null cyGraphs.results) $ do
      --   liftEff $ log "creating Cy.js"
      --   io.query $ H.action (CreateCy "http://localhost:8080/eles.json")
      --   liftEff $ log "created cy!"
