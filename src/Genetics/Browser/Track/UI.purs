module Genetics.Browser.Track.UI where

import Prelude

import Color (black)
import Color.Scheme.Clrs (blue, red)
import Control.Coroutine (Consumer, Producer, connect, runProcess)
import Control.Coroutine as Co
import Control.Monad.Aff (Aff, Fiber, Milliseconds(..), delay, finally, forkAff, killFiber, launchAff, launchAff_)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, makeVar, putVar, readVar, takeVar, tryTakeVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff (Eff, forE, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Random (random, randomRange)
import Control.Monad.Eff.Uncurried (EffFn4, runEffFn4)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (forever)
import DOM.Classy.Node (toNode)
import DOM.Classy.ParentNode (toParentNode)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Element as DOM
import DOM.Node.Node (appendChild) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import DOM.Node.Types (Element, Node)
import Data.Argonaut (Json, _Number, _Object, _String)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldMap, for_, null)
import Data.Foreign (Foreign, MultipleErrors, renderForeignError)
import Data.Lens (to, (^.), (^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (ala, over, unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Pair (Pair(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse, traverse_)
import Data.Tuple (Tuple(Tuple))
import Genetics.Browser.Track.Backend (Padding, RenderedTrack, browser, bumpFeatures, zipMapsWith)
import Genetics.Browser.Track.Demo (Annot, BedFeature, GWASFeature, annotLegendTest, demoTracks, getAnnotations, getGWAS, getGenes, gwasDraw, gwasGlyphTest, produceAnnots, produceGWAS, produceGenes)
import Genetics.Browser.Track.UI.Canvas (BrowserCanvas, TrackPadding, blankTrack, browserCanvas, debugBrowserCanvas, drawOnTrack, flipTrack, renderBatchGlyphs, renderBrowser, renderSingleGlyphs, renderTrack, trackViewScale, transTrack)
import Genetics.Browser.Types (Bp(..), ChrId(ChrId), Point)
import Genetics.Browser.Types.Coordinates (CoordSys(..), CoordSysView(..), ViewScale(..), _TotalSize, coordSys, normalizeView, pairSize, pixelsView, scalePairBy, scaleViewBy, showViewScale, translatePairBy, translateViewBy)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, Context2D, fillRect, getContext2D, setFillStyle)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, white)
import Graphics.Drawing as Drawing
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Simple.JSON (read, readJSON)
import Unsafe.Coerce (unsafeCoerce)


foreign import windowInnerSize :: forall e.
                                  Eff e Canvas.Dimensions

-- | Set an event to fire on the given button id
foreign import buttonEvent :: forall eff.
                              String
                           -> Eff eff Unit
                           -> Eff eff Unit


type ViewRange = Pair BigInt

data UpdateView =
    ScrollView Number
  | ZoomView Number
  | ModView (Pair BigInt -> Pair BigInt)


instance showUpdateView :: Show UpdateView where
  show (ScrollView x) = "(Scroll by " <> show x <> ")"
  show (ZoomView s) = "(Zoom by " <> show s <> ")"
  show _ = "(ModView)"


    -- TODO idk if this instance makes sense??? whatevs
instance semigroupUpdateView :: Semigroup UpdateView where
  append (ScrollView x1) (ScrollView x2) = ScrollView (x1 + x2)
  append (ZoomView s1)   (ZoomView s2)   = ZoomView   (s1 * s2)
  append _ y = y

instance monoidUpdateView :: Monoid UpdateView where
  mempty = ModView id


updateViewFold :: UpdateView
               -> CoordSysView
               -> CoordSysView
updateViewFold uv iv = case uv of
  ZoomView   x -> iv `scaleViewBy`     x
  ScrollView x -> iv `translateViewBy` x
  ModView f    -> over CoordSysView f iv


queueCmd :: AVar UpdateView -> UpdateView -> Eff _ Unit
queueCmd av cmd = launchAff_ $ putVar cmd av


btnScroll :: Number -> AVar UpdateView -> Eff _ Unit
btnScroll x av = do
  buttonEvent "scrollLeft"  $ queueCmd av $ ScrollView (-x)
  buttonEvent "scrollRight" $ queueCmd av $ ScrollView   x


btnZoom :: Number -> AVar UpdateView -> Eff _ Unit
btnZoom x av = do
  buttonEvent "zoomOut" $ queueCmd av $ ZoomView $ 1.0 + x
  buttonEvent "zoomIn"  $ queueCmd av $ ZoomView $ 1.0 - x



mouseChrSizes :: Array (Tuple ChrId BigInt)
mouseChrSizes =
  unsafePartial
  $ map (bimap ChrId (fromJust <<< BigInt.fromString))
      [ Tuple "1"   "195471971"
      , Tuple "2"   "182113224"
      , Tuple "3"   "160039680"
      , Tuple "4"   "156508116"
      , Tuple "5"   "151834684"
      , Tuple "6"   "149736546"
      , Tuple "7"   "145441459"
      , Tuple "8"   "129401213"
      , Tuple "9"   "124595110"
      , Tuple "10"  "130694993"
      , Tuple "11"  "122082543"
      , Tuple "12"  "120129022"
      , Tuple "13"  "120421639"
      , Tuple "14"  "124902244"
      , Tuple "15"  "104043685"
      , Tuple "16"  "98207768"
      , Tuple "17"  "94987271"
      , Tuple "18"  "90702639"
      , Tuple "19"  "61431566"
      , Tuple "X"   "171031299"
      , Tuple "Y"   "91744698"
      ]

-- runs console.time() with the provided string, returns the effect to stop the timer
foreign import timeEff :: forall eff. String -> Eff eff (Eff eff Unit)

type UIState e = { view :: AVar CoordSysView
                 , viewCmd :: AVar UpdateView
                 , viewReady :: AVar Unit
                 , renderFiber :: AVar (Fiber e Unit)
                 , cachedTracks :: AVar { cachedScale :: ViewScale
                                        , glyphs :: List (Array RenderedTrack) }
                 }



diffView :: CoordSysView -> CoordSysView -> CoordSysView
diffView v1 v2 =
  let (Pair l1 r1) = unwrap v1
      (Pair l2 r2) = unwrap v2
  in wrap $ Pair (l2 - l1) (r2 - r1)

showView :: CoordSysView -> String
showView (CoordSysView (Pair l r)) = BigInt.toString l <> " - " <> BigInt.toString r


peekUIState :: UIState _
            -> Eff _ Unit
peekUIState s = launchAff_ do
  view <- AVar.tryReadVar s.view
  case view of
    Nothing -> liftEff $ log "CoordSysView state empty"
    Just v  ->
      liftEff $ log $ "CoordSysView: " <> showView v


uiViewUpdate :: forall r.
                CoordSys _ BigInt
             -> Milliseconds
             -> { view :: AVar CoordSysView
                , viewCmd :: AVar UpdateView
                , viewReady :: AVar Unit| r }
             -> Aff _ Unit
uiViewUpdate cs timeout { view, viewCmd, viewReady } = do
  curCmdVar <- makeVar mempty

  let normView = normalizeView cs (BigInt.fromInt 2000)

      loop' updater = do
        cmd <- takeVar viewCmd

        killFiber (error "Resetting view update") updater
        liftEff $ log $ "forking view update"

        curCmd <- takeVar curCmdVar
        let cmd' = curCmd <> cmd
        putVar cmd' curCmdVar

        updater' <- forkAff do
            delay timeout
            liftEff $ log "Running view update"
            vr <- takeVar view

            liftEff $ log $ "   <  " <> showView vr
            let vr' = updateViewFold cmd' vr

            putVar vr' view
            liftEff $ log $ "   |  " <> show cmd'
            liftEff $ log $ "   >  " <> showView vr'
            liftEff $ log $ "diff: " <> showView (diffView vr vr')
            -- putVar (normView $ updateViewFold cmd' vr) view
            putVar unit viewReady
            takeVar curCmdVar *> putVar mempty curCmdVar

        loop' updater'

  loop' (pure unit)



renderLoop :: CoordSys _ _
           -> { tracks     :: Pair BigInt -> List (Array RenderedTrack)
              , relativeUI :: Pair BigInt -> Drawing
              , fixedUI :: Drawing }
           -> BrowserCanvas
           -> UIState _
           -> Aff _ Unit
renderLoop cSys browser canvas state = forever do

  _ <- takeVar state.viewReady

  csView <- readVar state.view
  -- if there's a rendering fiber running, we kill it
  traverse_ (killFiber (error "Resetting renderer"))
    =<< tryTakeVar state.renderFiber

  let uiScale = trackViewScale canvas csView

  -- if the view scale is unchanged, use the cached glyphs
  tracks' <- do
    cache <- AVar.tryTakeVar state.cachedTracks
    case cache of
      Just ct
        | ct.cachedScale == uiScale -> do
            AVar.putVar ct state.cachedTracks
            pure ct.glyphs
      _ -> do
        let cachedScale = uiScale
            glyphs = browser.tracks (unwrap csView)

        AVar.putVar {cachedScale, glyphs} state.cachedTracks
        pure glyphs

  -- fork a new renderFiber

  let (Pair offset _) = pixelsView uiScale csView

      relativeUI = browser.relativeUI (unwrap csView)
      fixedUI = browser.fixedUI

      ui ::  { tracks     :: List (Array RenderedTrack)
             , relativeUI :: Drawing
             , fixedUI :: Drawing }
      ui = { tracks: tracks'
           , relativeUI, fixedUI }

  renderFiber <- forkAff
                 $ renderBrowser (wrap 3.0) canvas offset ui

  putVar renderFiber state.renderFiber





-- TODO configure UI widths
runBrowser :: Conf -> BrowserCanvas -> Eff _ _
runBrowser config bc = launchAff $ do

  {width} <- liftEff $ windowInnerSize

  let height = config.browserHeight
      browserDimensions = {width, height}
      vScaleWidth = 60.0
      legendWidth = 120.0
      trackWidth = width - (vScaleWidth + legendWidth)



  let cSys :: CoordSys ChrId BigInt
      cSys = coordSys mouseChrSizes

  trackData <- do
    genes <- traverse (getGenes cSys) config.urls.genes
    gwas  <- traverse (getGWAS  cSys) config.urls.gwas
    rawAnnotations <-
      traverse (getAnnotations cSys) config.urls.annotations

    let annotations = zipMapsWith
                       (bumpFeatures (to _.score) (SProxy :: SProxy "score")
                         (Bp 1000000.0))
                       <$> gwas <*> rawAnnotations

    pure { genes, gwas, annotations }

  let initialView :: CoordSysView
      initialView = wrap $ Pair zero (cSys^._TotalSize)

  viewCmd <- makeEmptyVar
  liftEff $ do
    btnScroll 0.05 viewCmd
    btnZoom   0.10 viewCmd
    -- dragScroll trackWidth bCanvas viewCmd
    -- wheelZoom 0.02 viewCmd bCanvas.overlay

  view <- makeVar initialView
  viewReady <- makeVar unit
  renderFiber <- makeEmptyVar
  cachedTracks <- AVar.makeEmptyVar

  let initState :: UIState _
      initState = { view, viewCmd, viewReady, renderFiber, cachedTracks }

  let
      entries = foldMap annotLegendTest trackData.annotations
      legend = { width: legendWidth, entries }

      s = config.score

      vscale = { width: vScaleWidth, color: black
               , min: s.min, max: s.max, sig: s.sig }

      tracks = demoTracks vscale trackData

      padding = { horizontal: config.trackPadding.left
                , vertical: config.trackPadding.top }


      mainBrowser = browser cSys trackDimensions browserDimensions {legend, vscale} tracks

      viewTimeout :: Milliseconds
      viewTimeout = wrap 100.0

  liftEff do
    setWindow "mainBrowser" mainBrowser
    setWindow "peekView" (peekUIState initState)

  _ <- forkAff $ uiViewUpdate cSys viewTimeout initState
  _ <- forkAff $ renderLoop cSys mainBrowser bc initState


  pure unit



type DataURLs = { gwas        :: Maybe String
                , annotations :: Maybe String
                , genes       :: Maybe String
                }

type Conf = { browserHeight :: Number
            , trackPadding :: TrackPadding
            , score :: { min :: Number, max :: Number, sig :: Number }
            , urls :: DataURLs
            }

foreign import setWindow :: forall e a. String -> a -> Eff e Unit


initBrowser :: Foreign -> Eff _ _
initBrowser rawConfig = do

  el' <- do
    doc <- DOM.htmlDocumentToDocument
           <$> (DOM.document =<< DOM.window)
    DOM.querySelector (wrap "#browser") (toParentNode doc)

  case read rawConfig :: Either MultipleErrors Conf of
    Left errs -> traverse_ (log <<< renderForeignError) errs
    Right c   -> do
      case el' of
        Nothing -> log "Could not find element '#browser'"
        Just el -> do

          {width} <- windowInnerSize
          let dimensions = { width, height: c.browserHeight }
          bc <- browserCanvas dimensions c.trackPadding el

          debugBrowserCanvas "debugBC" bc

          let drawR :: Number -> Number -> Eff _ Unit
              drawR x y = drawOnTrack bc \ctx -> do
                _ <- setFillStyle "red" ctx
                _ <- fillRect ctx { x, y, w: 20.0, h: 20.0 }
                pure unit


          setWindow "drawRect" drawR
          setWindow "flipTrack" (flipTrack bc)
          setWindow "blankTrack" (blankTrack bc)

          log $ unsafeStringify c
          void $ runBrowser c bc




-- TODO this could almost certainly be done better
chunkConsumer :: forall m a.
                 Foldable m
              => Monoid (m a)
              => AVar (m a)
              -> Consumer (m a) (Aff _) Unit
chunkConsumer av = Co.consumer \m ->
  if null m
     then pure $ Just unit
     else do
       sofar <- takeVar av
       putVar (sofar <> m) av
       delay (wrap 20.0)
       pure Nothing


type TrackVar a = AVar (Map ChrId (Array a))
type TrackProducer eff a = Producer (Map ChrId (Array a)) (Aff eff) Unit

-- Feels like this one takes care of a bit too much...
fetchLoop1 :: forall a.
              (Maybe (Aff _ (TrackProducer _ a)))
           -> Aff _ (TrackVar a)
fetchLoop1 Nothing = AVar.makeEmptyVar
fetchLoop1 (Just startProd) = do
  prod <- startProd
  avar <- makeVar mempty
  _ <- forkAff $ runProcess $ prod `connect` chunkConsumer avar
  pure avar

-- | Starts threads that fetch & parse each of the provided tracks,
-- | filling an AVar over time per track, which can be used by other parts of the application
-- | (read only, should be a newtype)
fetchLoop :: CoordSys ChrId BigInt
          -> DataURLs
          -> Aff _
               { gwas :: TrackVar (GWASFeature ())
               , genes :: TrackVar BedFeature
               , annotations :: TrackVar (Annot ())
               }
fetchLoop cs urls = do
  gwas <-        fetchLoop1 $ produceGWAS   cs <$> urls.gwas
  annotations <- fetchLoop1 $ produceAnnots cs <$> urls.annotations
  genes <-       fetchLoop1 $ produceGenes  cs <$> urls.genes
  pure { gwas, genes, annotations }
