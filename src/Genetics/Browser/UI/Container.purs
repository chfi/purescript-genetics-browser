module Genetics.Browser.UI.Container
       where

import Prelude

import Control.Coroutine as CR
import Control.Error.Util (note)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut (class EncodeJson, JObject, Json, _Array, _Number, _Object, _String, decodeJson, encodeJson, jsonEmptyObject, jsonParser, (.?), (~>))
import Data.Array (mapMaybe, null, uncons)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Foldable (foldMap, length, sequence_)
import Data.Foreign (Foreign, renderForeignError)
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Int (round)
import Data.Lens (re, (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Options (Options, (:=))
import Data.Predicate (Predicate(..))
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Debug.Trace (traceShow)
import Genetics.Browser.Biodalliance (initBD, setLocation)
import Genetics.Browser.Biodalliance as Biodalliance
import Genetics.Browser.Biodalliance.Config (RendererInfo, renderers, sources)
import Genetics.Browser.Biodalliance.Source (Source)
import Genetics.Browser.Biodalliance.Source as Source
import Genetics.Browser.Biodalliance.Types (BD, Biodalliance, Renderer)
import Genetics.Browser.Config (BrowserConfig(..), parseBrowserConfig)
import Genetics.Browser.Config.Track (validateConfigs)
import Genetics.Browser.Cytoscape as Cytoscape
import Genetics.Browser.Cytoscape.Collection (filter, isEdge, targetNodes)
import Genetics.Browser.Cytoscape.Collection as CyCollection
import Genetics.Browser.Cytoscape.Types (CY, Cytoscape, Element, elementJObject, elementJson)
import Genetics.Browser.Events (Location(..), Range(..))
import Genetics.Browser.Events.TrackSink (SinkConfig, TrackSink, forkTrackSink, makeTrackSink, makeTrackSinks)
import Genetics.Browser.Events.TrackSource (SourceConfig, TrackSource, emptyTrackSource, makeTrackSource, makeTrackSources, runTrackSource)
import Genetics.Browser.Events.Types (Event)
import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Renderer.Lineplot as QTL
import Genetics.Browser.UI.Biodalliance as UIBD
import Genetics.Browser.UI.Cytoscape as UICy
import Genetics.Browser.Units (Bp(Bp), Chr(..), _Bp, _BpMBp, _Chr, _MBp, bp)
import Global.Unsafe (unsafeStringify)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import IPFS as IPFS
import IPFS.Files as Files
import IPFS.Types (IPFSPath(..))
import Node.Encoding (Encoding, Encoding(..))
import Node.Stream (Readable, readString)
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)


type BDEventEff eff = (console :: CONSOLE, bd :: BD, avar :: AVAR | eff)
type CyEventEff eff = (console :: CONSOLE, cy :: CY, avar :: AVAR | eff)


subscribeBDEvents :: ∀ r.
                     (TrackSource Event)
                  -> Biodalliance
                  -> BusRW Event
                  -> Eff _ Unit
subscribeBDEvents h bd bus =
  Biodalliance.addFeatureListener bd $ \obj -> do
    let evs = runTrackSource h (unwrap obj)
    traverse_ (\x -> Aff.launchAff $ Bus.write x bus) evs


subscribeCyEvents :: ∀ r.
                     (TrackSource Event)
                  -> Cytoscape
                  -> BusRW Event
                  -> Eff _ Unit
subscribeCyEvents h cy bus =
  Cytoscape.onClick cy $ \obj -> do
    let evs = runTrackSource h (unwrap obj)
    traverse_ (\x -> Aff.launchAff $ Bus.write x bus) evs


qtlGlyphify :: LinePlotConfig -> Renderer
qtlGlyphify = QTL.render

gwasGlyphify :: Renderer
gwasGlyphify = GWAS.render


data Track = BDTrack | CyTrack

type State = Unit

data Query a
  = CreateBD (∀ eff. HTMLElement -> Eff (bd :: BD | eff) Biodalliance) a
  | PropagateMessage Message a
  | BDScroll Bp a
  | BDJump Chr Bp Bp a
  | CreateCy String a
  | ResetCy a

data Message
  = BDInstance Biodalliance
  | CyInstance Cytoscape
  -- | WithCy Cytoscape

type ChildSlot = Either2 UIBD.Slot UICy.Slot

type ChildQuery = UIBD.Query <\/> UICy.Query <\/> Const Void
type Effects eff = UIBD.Effects (UICy.Effects eff)


component :: ∀ eff. H.Component HH.HTML Query Unit Message (Aff (Effects eff))
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
      ]


  handleBDMessage :: UIBD.Message -> Maybe (Query Unit)
  handleBDMessage UIBD.Initialized = Nothing
  handleBDMessage (UIBD.SendBD bd) = Just $ H.action $ PropagateMessage (BDInstance bd)

  -- TODO the event source track should be handled automatically somehow
  handleCyMessage :: UICy.Output -> Maybe (Query Unit)
  handleCyMessage (UICy.SendCy cy) = Just $ H.action $ PropagateMessage (CyInstance cy)
  handleCyMessage (UICy.SendEvent) = Nothing


  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Aff (Effects eff))
  eval = case _ of
    CreateBD bd next -> do
      _ <- H.query' CP.cp1 UIBD.Slot $ H.action (UIBD.Initialize bd)
      pure next

    PropagateMessage msg next -> do
      case msg of
        BDInstance _ -> liftEff $ log "propagating BD"
        CyInstance _ -> liftEff $ log "propagating Cy"
      H.raise msg
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



qtlRenderer :: Int -> RendererInfo
qtlRenderer h = { renderer: qtlGlyphify { minScore: 4.0
                                        , maxScore: 6.0
                                        , color: "#ff0000"
                                        }
                , canvasHeight: h
                }

gwasRenderer :: Int -> RendererInfo
gwasRenderer h = { renderer: gwasGlyphify
                 , canvasHeight: h
                 }


createSource :: forall eff a. (Chr -> Bp -> Bp -> Aff eff a) -> Source a
createSource = Source.createSource


type BasicFeature r = { chr :: String
                      , min :: Int
                      , max :: Int
                      | r
                      }

testPath = "QmQwtu1Lr1R3syY15Ak2U85NZjbWrtda8pE8Dp4mnQLPEx"


parseScoreFeature :: Json -> Either String (BasicFeature (score :: Number))
parseScoreFeature j = do
  obj <- decodeJson j
  chr <- (obj .? "chr")
  min <- round <$> obj .? "min"
  max <- round <$> obj .? "max"
  score <- (obj .? "score")
  pure {chr, min, max, score}


foreign import setBDRef :: ∀ eff. Biodalliance -> Eff eff Unit


foreign import bdTrackSinkConfig :: forall eff.
                                    Array (SinkConfig (Biodalliance -> Eff (BDEventEff eff) Unit))

-- foreign import bdTrackSourceConfig :: Array SourceConfig

-- foreign import cyGraphSinkConfig :: forall

foreign import cyGraphSourceConfig :: Array SourceConfig

foreign import testRange :: Json

filterCytoscape :: Cytoscape -> Predicate Json -> Eff _ Unit
filterCytoscape cy p = do
  g <- Cytoscape.graphGetCollection cy
  let g' = CyCollection.filter (elementJson >$< p) g
  Cytoscape.graphRemoveCollection g' *> pure unit


main :: Foreign -> Eff _ Unit
main fConfig = HA.runHalogenAff do
  case runExcept $ parseBrowserConfig fConfig of
    Left e -> liftEff $ do
      log "Invalid browser configuration:"
      sequence_ $ log <<< renderForeignError <$> e

    Right (BrowserConfig config) -> do
      let {bdTracks, cyGraphs} = validateConfigs config.tracks

          opts' =  sources := bdTracks.results
                <> renderers := config.bdRenderers

      liftEff $ log $ "BDTrack errors: " <> foldMap ((<>) ", ") bdTracks.errors
      liftEff $ log $ "CyGraph errors: " <> foldMap ((<>) ", ") cyGraphs.errors

      let mkBd :: (∀ eff. HTMLElement -> Eff (bd :: BD | eff) Biodalliance)
          mkBd = initBD opts' config.wrapRenderer config.browser


      liftEff $ log "running main"
      HA.awaitLoad
      el <- HA.selectElement (wrap "#psgbHolder")

      case el of
        Nothing -> do
          liftEff $ log "no element for browser!"
        Just el' -> do

          io <- runUI component unit el'

          busFromBD <- Bus.make
          busFromCy <- Bus.make

          let bdTrackSink = makeTrackSinks bdTrackSinkConfig
              bdTrackSource = (makeTrackSources <<< _.bdEventSources)
                              =<< note "No events configured" (config.events)

          when (not null bdTracks.results) do
            io.subscribe $ CR.consumer $ case _ of
              BDInstance bd -> do
                liftEff $ log "attaching BD event handlers"

                case bdTrackSink of
                  Nothing -> liftEff $ log "No BD TrackSink!"
                  Just ts -> forkTrackSink ts bd busFromCy *> pure unit

                case bdTrackSource of
                  Left err -> liftEff do
                    log "No BD TrackSource! Error:"
                    log err
                  Right ts -> liftEff do
                    subscribeBDEvents ts bd busFromBD

                liftEff $ setBDRef bd
                pure Nothing
              _ -> pure $ Just unit
            liftEff $ log "creating BD"
            io.query $ H.action (CreateBD mkBd)
            liftEff $ log "created BD!"


          liftEff $ log $ "cytoscape enabled: " <> show (not null cyGraphs.results)

          -- let cyGraphSink = makeTrackSinks cyGraphSinkConfig
          let cyGraphSource = makeTrackSources cyGraphSourceConfig

          case uncons cyGraphs.results of
            Nothing -> pure unit
            Just {head, tail} -> do
              io.subscribe $ CR.consumer $ case _ of
                CyInstance cy -> do
                  liftEff $ log "attaching Cy event handlers"

                  case cyGraphSource of
                    Left err -> liftEff do
                      log "No Cy graph source!"
                      log err
                    Right gs -> liftEff $ subscribeCyEvents gs cy busFromCy

                  pure Nothing
                _ -> pure $ Just unit

              liftEff $ log "creating Cy.js"
              io.query $ H.action (CreateCy $ _.elementsUri <<< unwrap $ head)
              liftEff $ log "created cy!"
