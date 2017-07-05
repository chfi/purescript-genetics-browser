module Genetics.Browser.UI.Container
       where

import Prelude
import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot as QTL
import Genetics.Browser.UI.Biodalliance as UIBD
import Genetics.Browser.UI.Cytoscape as UICy
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut (_Array)
import Data.Array (foldr, null, (:))
import Data.Const (Const)
import Data.Either (Either(Right, Left))
import Data.Either.Nested (Either2)
import Data.Foldable (foldMap)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Lens ((^?))
import Data.Maybe (Maybe(Just, Nothing), isNothing, maybe)
import Data.Newtype (wrap)
import Data.Options (Options, (:=))
import Genetics.Browser.Biodalliance (RendererInfo, initBD, renderers, sources)
import Genetics.Browser.Config (BrowserConfig(..))
import Genetics.Browser.Config.Track (CyGraphConfig, validateBDConfig, validateCyConfig)
import Genetics.Browser.Events (JsonEvent)
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Types (BD, Biodalliance, Renderer)
import Genetics.Browser.Units (Bp(Bp), Chr)
import Global.Unsafe (unsafeStringify)
import Halogen.VDom.Driver (runUI)




qtlGlyphify :: LinePlotConfig -> Renderer
qtlGlyphify = QTL.render

gwasGlyphify :: Renderer
gwasGlyphify = GWAS.render

data Track = BDTrack | CyTrack

type State = Unit

data Query a
  = Nop a
  | CreateBD (∀ eff. HTMLElement -> Eff (bd :: BD | eff) Biodalliance) a
  | BDScroll Bp a
  | BDJump Chr Bp Bp a
  | CreateCy String a
  | ResetCy a
  | DistEvent Track JsonEvent a

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

  addCyGraph :: Maybe CyGraphConfig -> _
  addCyGraph = case _ of
    Nothing -> []
    Just cy -> [HH.div [] [HH.slot' CP.cp2 UICy.Slot UICy.component unit handleCyMessage]]

  handleBDMessage :: UIBD.Message -> Maybe (Query Unit)
  handleBDMessage UIBD.Initialized = Just $ ResetCy unit
  handleBDMessage (UIBD.SendEvent obj) = Just $ DistEvent BDTrack obj unit

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

    DistEvent from ev next -> do
      liftEff $ log "container is distributing event:"
      liftEff $ log $ unsafeStringify ev

      _ <- case from of
        BDTrack ->
          H.query' CP.cp2 UICy.Slot $ H.action (UICy.RecvEvent ev)
        CyTrack ->
          H.query' CP.cp1 UIBD.Slot $ H.action (UIBD.RecvEvent ev)

      pure next


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
  when (isNothing $ tracks ^? _Array) $ liftEff $ log "Tracks config is not an array"

  let validated = maybe [] (map validateBDConfig) $ tracks ^? _Array
      validatedCy = maybe [] (map validateCyConfig) $ tracks ^? _Array

      {errors, tracks} = foldr (\c {errors, tracks} ->
                                 case c of Left  e -> {errors: (e : errors), tracks}
                                           Right t -> {errors, tracks: (t : tracks)}
                               ) { errors: [], tracks: [] } validated

      opts' = bdOpts <> sources := tracks

  liftEff $ log $ "Track errors: " <> foldMap ((<>) ", ") errors

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
      liftEff $ log "creating BD"
      io.query $ H.action (CreateBD mkBd)
      liftEff $ log "created BD!"
      liftEff $ log $ "cytoscape enabled: " <> show (not null validatedCy)
      when (not null validatedCy) $ do
        liftEff $ log "creating Cy.js"
        io.query $ H.action (CreateCy "http://localhost:8080/eles.json")
        liftEff $ log "created cy!"
