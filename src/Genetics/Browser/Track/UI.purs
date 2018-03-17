module Genetics.Browser.Track.UI where

import Prelude

import Color (black)
import Control.Coroutine (Consumer, Producer, connect, runProcess)
import Control.Coroutine as Co
import Control.Monad.Aff (Aff, Fiber, Milliseconds, delay, finally, forkAff, killFiber, launchAff, launchAff_)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, makeVar, putVar, readVar, takeVar, tryTakeVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
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
import Data.Newtype (wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Pair (Pair(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(Tuple))
import Genetics.Browser.Track.Backend (Padding, RenderedTrack, browser, bumpFeatures, zipMapsWith)
import Genetics.Browser.Track.Demo (Annot, BedFeature, GWASFeature, annotLegendTest, demoTracks, getAnnotations, getGWAS, getGenes, produceAnnots, produceGWAS, produceGenes)
import Genetics.Browser.Track.UI.Canvas (BrowserCanvas, TrackPadding, blankTrack, browserCanvas, debugBrowserCanvas, drawOnTrack, flipTrack)
import Genetics.Browser.Types (Bp(..), ChrId(ChrId), Point)
import Genetics.Browser.Types.Coordinates (CoordSys(..), _TotalSize, coordSys, pairSize, scalePairBy, translatePairBy)
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

-- set an event to fire on the given button id

foreign import buttonEvent :: forall eff.
                              String
                           -> Eff eff Unit
                           -> Eff eff Unit


type ViewRange = Pair BigInt

data UpdateView =
    ScrollView Number
  | ZoomView Number
  | ModView (ViewRange -> ViewRange)


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
               -> ViewRange
               -> ViewRange
updateViewFold uv iv@(Pair l r) = case uv of
  ZoomView   x -> iv `scalePairBy`     x
  ScrollView x -> iv `translatePairBy` x
  ModView f    -> f iv


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

viewMachine :: forall r.
               CoordSys _ _
            -> Milliseconds
            -> { viewRange :: AVar ViewRange
               , viewCmds :: AVar UpdateView | r }
            -> AVar Unit
            -> Aff _ Unit
viewMachine cs timeout { viewRange, viewCmds } ready = do

  curCmdVar <- makeVar mempty

  let limL = zero
      limR = cs ^. _TotalSize

      loop' updater = do
        cmd <- takeVar viewCmds

        killFiber (error "Resetting view update") updater

        curCmd <- takeVar curCmdVar
        let cmd' = curCmd <> cmd
        putVar cmd' curCmdVar

        updater' <- forkAff do
            delay timeout

            vr <- takeVar viewRange
                -- update view using received cmd, limiting view to provided boundaries
            let (Pair l' r') = updateViewFold cmd' vr
                -- TODO bake into updateViewFold or handle at a better place/in a nicer way
                -- TODO handle minimum size
                len = r' - l'
                vr' = case l' < limL, r' > limR of
                        true, false  -> Pair limL len
                        false, true  -> Pair (limR - len) limR
                        true, true   -> Pair limL limR
                        false, false -> Pair l' r'

            putVar vr' viewRange
            putVar unit ready
            takeVar curCmdVar *> putVar mempty curCmdVar

        loop' updater'

  loop' (pure unit)



-- TODO the browser state could really stand to be factored into a proper type
-- TODO bake the ready-state into the browser state
renderLoop :: CoordSys _ _
           -> { tracks     :: Pair BigInt -> List (Array RenderedTrack)
              , relativeUI :: Pair BigInt -> Drawing
              , fixedUI :: Drawing }
           -> Number
           -> BrowserCanvas
           -> { viewRange   :: AVar ViewRange
              , viewCmds    :: AVar UpdateView
              , renderFiber :: AVar (Fiber _ Unit)
              , cachedTracks :: AVar { viewSize :: BigInt
                                     , glyphs :: List (Array RenderedTrack) }
              }
           -> AVar Unit
           -> Aff _ Unit
renderLoop cSys browser trackDisplayWidth canvas state ready = pure unit
{-
renderLoop cSys browser trackDisplayWidth canvas state ready = forever do

  _ <- takeVar ready

  visible <- readVar state.viewRange
  -- if there's a rendering fiber running, we kill it
  traverse_ (killFiber (error "Resetting renderer"))
    =<< tryTakeVar state.renderFiber


  -- if the view scale is unchanged, use the cached glyphs
  tracks <- do
    cache <- AVar.tryTakeVar state.cachedTracks
    case cache of
      Just ct
        | ct.viewSize == pairSize visible -> do
            AVar.putVar ct state.cachedTracks
            pure ct.glyphs
      _ -> do
        let viewSize = pairSize visible
            glyphs = browser.tracks visible

        AVar.putVar {viewSize, glyphs} state.cachedTracks
        pure glyphs


  -- fork a new renderFiber
  renderFiber <- forkAff do

    trackCtx   <- liftEff $ getContext2D canvases.track

    let scale = BigInt.toNumber (pairSize visible) / trackDisplayWidth
        (Pair offset _) =
          ((_/scale) <<< BigInt.toNumber) <$> visible

        -- used instead of withContext to be able to thread the rendering and
        -- reset the offset if the fiber is killed
        shiftView x =
          void $ Canvas.translate
            { translateX: x, translateY: zero } trackCtx

    liftEff do
      overlayCtx <- getContext2D canvases.overlay
      Drawing.render overlayCtx browser.fixedUI

      trackCtx <- getContext2D canvases.track
      {width, height} <- Canvas.getCanvasDimensions canvases.track
      Drawing.render trackCtx
        $ filled (fillColor white) $ rectangle 0.0 0.0 width height

      shiftView (-offset)
      Drawing.render trackCtx $ browser.relativeUI visible

    finally (liftEff $ shiftView offset) do
      renderGlyphs visible scale tracks canvases

  putVar renderFiber state.renderFiber




-- TODO this should be smarter and have fewer magic numbers
renderBatch :: CanvasElement
            -> { drawing :: Drawing, points :: Array Point }
            -> Context2D
            -> Eff _ Unit
renderBatch buffer {drawing, points} ctx = do
  -- hardcoded glyph sizes for now.
  let r = 13.0
      d = r * 2.0
      dim = { width: 100.0, height: d }
                          -- width even more hardcoded to deal with long text!
  _ <- Canvas.setCanvasDimensions dim buffer
  bfrCtx <- Canvas.getContext2D buffer
  Drawing.render bfrCtx $ Drawing.translate r r drawing
  let img = Canvas.canvasElementToImageSource buffer
  runEffFn4 drawImageMany buffer ctx dim points


renderGlyphs :: Pair BigInt
             -> Number
             -> List (Array RenderedTrack)
             -> BrowserCanvas
             -> Aff _ Unit
renderGlyphs vw@(Pair l _) viewScale ts canvases = do
  let vw'@(Pair pxL pxR) = map (\x -> (BigInt.toNumber x / viewScale)) vw
  -- Predicates to filter out glyphs that would not be visible on screen
     -- Hack to render more of the canvas just to be sure
      pxL' = pxL - (pxR - pxL)
      pxR' = pxR + (pxR - pxL)
      predB p = p.x - 10.0 > pxL'
             && p.x + 10.0 < pxR'
      predS s = s.width >= one
             && s.point.x - s.width > pxL'
             && s.point.x + s.width < pxR'

  trackCtx <- liftEff $ Canvas.getContext2D canvases.track

  for_ ts \track -> do
    for_ track \t -> do
      case t of
        Left {drawing, points}  -> do
          let points' = filter predB points
          liftEff $
            renderBatch canvases.buffer {drawing, points: points'} trackCtx

        Right gs -> do
          let gs' = filter predS gs
          liftEff $ foreachE gs' \s -> do
            Drawing.render trackCtx
              $ Drawing.translate s.point.x s.point.y
              $ s.drawing unit

      -- delay to give the UI thread a moment
      delay (wrap 3.0)
-}


-- TODO configure UI widths
runBrowser :: Conf -> Eff _ _
runBrowser config = launchAff $ do

  {width} <- liftEff $ windowInnerSize

  let height = config.browserHeight
      browserDimensions = {width, height}
      vScaleWidth = 60.0
      legendWidth = 120.0
      trackWidth = width - (vScaleWidth + legendWidth)


  -- bCanvas <- do
  --   doc <- liftEff $ DOM.htmlDocumentToDocument
  --          <$> (DOM.document =<< DOM.window)
  --   cont <- liftEff $ DOM.querySelector (wrap "#browser") (toParentNode doc)

  --   case cont of
  --     Nothing -> throwError $ error "Could not find browser element"
  --     Just el -> liftEff do
  --       createBrowserCanvas
  --         el browserDimensions
  --         { translateX: vScaleWidth
  --         , translateY: zero }


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

  let initialView :: Pair BigInt
      initialView = Pair zero (cSys^._TotalSize)

  viewCmds <- makeEmptyVar
  liftEff $ do
    btnScroll 0.05 viewCmds
    btnZoom   0.10 viewCmds
    -- dragScroll trackWidth bCanvas viewCmds
    -- wheelZoom 0.02 viewCmds bCanvas.overlay

  viewRange <- makeVar initialView

  renderFiber <- makeEmptyVar

  let
      entries = foldMap annotLegendTest trackData.annotations
      legend = { width: legendWidth, entries }
      s = config.score
      vscale = { width: vScaleWidth, color: black
               , min: s.min, max: s.max, sig: s.sig }
      tracks = demoTracks vscale trackData
      padding = { horizontal: config.trackPadding.left
                , vertical: config.trackPadding.top }
      mainBrowser = browser cSys browserDimensions padding {legend, vscale} tracks
      viewTimeout = wrap 100.0

  viewReady <- makeVar unit

  cachedTracks <- AVar.makeEmptyVar

  _ <- forkAff $ viewMachine cSys viewTimeout {viewRange, viewCmds} viewReady
  -- _ <- forkAff
  --        $ renderLoop cSys mainBrowser
  --          trackWidth bCanvas
  --          { viewCmds, viewRange, renderFiber, cachedTracks }
  --          viewReady

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
