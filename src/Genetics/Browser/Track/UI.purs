module Genetics.Browser.Track.UI where

import Prelude

import Color (black)
import Control.Coroutine (Consumer, Process, Producer, connect, runProcess, ($$))
import Control.Coroutine as Co
import Control.Monad.Aff (Aff, Fiber, delay, forkAff, killFiber, launchAff, launchAff_)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, makeVar, putVar, readVar, takeVar, tryTakeVar)
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
import Data.Bifunctor (bimap, lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Filterable (filter, partitioned)
import Data.Foldable (class Foldable, fold, foldMap, for_, length, null)
import Data.Lens (to, (^.), (^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing, Just), fromJust, fromMaybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Pair (Pair(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Variant (Variant)
import FRP.Event (Event)
import Genetics.Browser.Track.Backend (Padding, RenderedTrack, browser, bumpFeatures, zipMapsWith)
import Genetics.Browser.Track.Bed (ParsedLine)
import Genetics.Browser.Track.Demo (BedFeature, GWASFeature, Annot, annotLegendTest, demoTracksBed, getAnnotations, getBedGenes, getGWAS, produceAnnots, produceGWAS, produceGenes)
import Genetics.Browser.Types (Bp(..), ChrId(ChrId), Point)
import Genetics.Browser.Types.Coordinates (CoordSys, _TotalSize, coordSys, pairSize, scalePairBy, translatePairBy)
import Graphics.Canvas (CanvasElement, Context2D, getContext2D)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, white)
import Graphics.Drawing as Drawing
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Unsafe.Coerce (unsafeCoerce)



foreign import getScreenSize :: forall eff. Eff eff { width :: Number, height :: Number }

-- 1st element is a backbuffer, 2nd the one shown on screen
foreign import scrollCanvas :: forall eff.
                               CanvasElement
                            -> CanvasElement
                            -> Point
                            -> Eff eff Unit


foreign import canvasDragImpl :: forall eff.
                                 CanvasElement
                              -> ( { during :: Nullable Point
                                   , total :: Nullable Point } -> Eff eff Unit )
                              -> Eff eff Unit

canvasDrag :: (Either Point Point -> Eff _ Unit)
           -> CanvasElement
           -> Eff _ Unit
canvasDrag f el =
  let toEither g {during, total} = case toMaybe during of
        Just p  -> g $ Right p
        Nothing -> g $ Left $ fromMaybe {x:zero, y:zero} $ toMaybe total
  in canvasDragImpl el (toEither f)

-- creates a new CanvasElement, not attached to the DOM and thus not visible
foreign import newCanvas :: forall eff.
                            { width :: Number, height :: Number }
                         -> Eff eff CanvasElement


-- set an event to fire on the given button id

foreign import buttonEvent :: forall eff.
                              String
                           -> Eff eff Unit
                           -> Eff eff Unit


foreign import setViewUI :: forall eff. String -> Eff eff Unit


type ViewRange = Pair BigInt

data UpdateView =
    ScrollView Number
  | ZoomView Number
  | ModView (ViewRange -> ViewRange)

updateViewFold :: UpdateView
               -> ViewRange
               -> ViewRange
updateViewFold uv iv@(Pair l r) = case uv of
  ZoomView   x -> iv `scalePairBy`  x
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


-- TODO handle normalization of view
-- TODO sync up graphical canvas-scrolling with actual browser viewstate
dragScroll :: Number
           -> BrowserCanvas
           -> AVar UpdateView
           -> Eff _ Unit
dragScroll width cnv av =
  let f = case _ of
        Left  {x,y} -> queueCmd av $ ScrollView $ g (-x)
        Right {x,y} -> scrollCanvas cnv.buffer cnv.track {x: -x, y: zero}
      g x = x / width
  in canvasDrag f cnv.overlay



foreign import canvasWheelEvent :: CanvasElement -> Event Number

scrollZoomEvent :: CanvasElement -> Event UpdateView
scrollZoomEvent el = map (ZoomView <<< f) $ canvasWheelEvent el
  where f :: Number -> Number
        f dY = let d' = 10000.0
                   n = dY * d'
                   d = d' * 100.0
               in n / d


-- TODO differentiate scroll buffer & rendering buffer
type BrowserCanvas = { buffer  :: CanvasElement
                     , track   :: CanvasElement
                     , overlay :: CanvasElement
                     }

createBrowserCanvas :: Element
                    -> { width :: Number, height :: Number }
                    -> Canvas.TranslateTransform
                    -> Eff _ BrowserCanvas
createBrowserCanvas el dim trackTT = do
  let node :: CanvasElement -> Node
      node = unsafeCoerce
      element :: CanvasElement -> Element
      element = unsafeCoerce

  buffer  <- newCanvas dim
  track   <- newCanvas dim
  -- Translate the origin of the Track canvas;
  -- useful to place the origin within the vertical padding and horizontal UI elements
  void $ Canvas.getContext2D track >>= Canvas.translate trackTT

  overlay <- newCanvas dim

  DOM.setId (wrap "buffer")  (element buffer)
  DOM.setId (wrap "track")   (element track)
  DOM.setId (wrap "overlay") (element overlay)

  DOM.setAttribute "style" (   "width: "  <> show dim.width  <> "px"
                          <> "; height: " <> show dim.height <> "px"
                          <> "; position:relative"
                          <> "; border: 1px solid black; display: block; margin: 0; padding: 0"
                          ) el

  let css i = "position:absolute; z-index: " <> i

  DOM.setAttribute "style" (css "1") (element track)
  DOM.setAttribute "style" (css "2") (element overlay)

  _ <- DOM.appendChild (node track)   (toNode el)
  _ <- DOM.appendChild (node overlay) (toNode el)

  pure { buffer, track, overlay }


cSys :: CoordSys ChrId BigInt
cSys = coordSys mouseChrSizes

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
      , Tuple "X"   "17103129"
      , Tuple "Y"   "9174469"
      ]


type DataURLs = { gwas        :: Maybe String
                , annotations :: Maybe String
                , genes       :: Maybe String
                }


type Conf = { browserHeight :: Number
            , padding :: Padding
            , score :: { min :: Number, max :: Number, sig :: Number }
            , urls :: DataURLs
            }


foreign import timeFun :: forall eff a. (Unit -> a) -> Eff eff Unit



foreign import drawImageMany :: forall eff a.
                                EffFn4 eff
                                CanvasElement
                                Context2D
                                { width :: Number, height :: Number }
                                (Array Point)
                                Unit



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


type BrowserState = { visible  :: ViewRange }
                    -- , rendered :: ViewRange }


renderLoop :: CoordSys _ _
           -> { tracks     :: Pair BigInt -> List (Array RenderedTrack)
              , relativeUI :: Pair BigInt -> Drawing
              , fixedUI :: Drawing }
           -> Number
           -> BrowserCanvas
           -> { viewState   :: AVar BrowserState
              , viewCmds    :: AVar UpdateView
              , renderFiber :: AVar (Fiber _ _)}
           -> Aff _ _
renderLoop cSys browser trackDisplayWidth canvases state = forever do

  vState <- takeVar state.viewState

  traverse_ (killFiber (error "Resetting renderer"))
    =<< tryTakeVar state.renderFiber

  let scale = BigInt.toNumber (pairSize vState.visible) / trackDisplayWidth
      (Pair l _ ) = vState.visible
      offset = BigInt.toNumber l / scale
      tracks = browser.tracks vState.visible

  liftEff do
    log $ "current scale: " <> show scale <> " bps/pixel"

  renderer <- forkAff $ renderGlyphs vState.visible scale tracks canvases

  liftEff $ do
    trackCtx   <- getContext2D canvases.track
    overlayCtx <- getContext2D canvases.overlay

    -- Shift canvas so we render to the visible part
    -- this shit ugly af tho, all of this gotta get redone~~

    void $ Canvas.withContext trackCtx do
      void $ Canvas.translate
        { translateX: (-offset), translateY: zero } trackCtx
      Drawing.render trackCtx $ browser.relativeUI vState.visible

    Drawing.render overlayCtx browser.fixedUI

  -- wait until UI has been clicked, view scrolled, etc.
  cmd <- takeVar state.viewCmds

      -- update view using received cmd, limiting view to coordsys
  let (Pair l' r') = updateViewFold cmd vState.visible
      newState =
        Pair (max zero l')
             (min (max r'  -- arbitrary minimum of 400 units shown, so things don't go too crazy when zoomed in
                      (l' + BigInt.fromInt 400))
                  (cSys^._TotalSize))

  putVar { visible: newState } state.viewState

  liftEff $ log $ "new view: " <> show newState

  pure unit


renderGlyphs :: Pair BigInt
             -> Number
             -> List (Array RenderedTrack)
             -> BrowserCanvas
             -> Aff _ Unit
renderGlyphs vw@(Pair l _) viewScale ts canvases = do

  {width, height} <- liftEff $ Canvas.getCanvasDimensions canvases.track

  let bg = filled (fillColor white) $ rectangle 0.0 0.0 width height

  trackCtx   <- liftEff $ getContext2D canvases.track
  overlayCtx <- liftEff $ getContext2D canvases.overlay

  let offset = BigInt.toNumber l / viewScale
      vw'@(Pair pxL pxR) = map (\x -> (BigInt.toNumber x / viewScale)) vw

  liftEff do
    log $ "offset: " <> show offset
    log $ "pixels view: " <> show vw'
    Drawing.render trackCtx bg

  -- Shift canvas so we render to the visible part
  void $ liftEff $ Canvas.translate { translateX: (-offset), translateY: zero } trackCtx

  -- Predicates to filter out glyphs that would not be visible on screen
     -- Hack to render more of the canvas just to be sure
  let pxL' = pxL - (pxR - pxL)
      pxR' = pxR + (pxR - pxL)
      predB p = p.x - 10.0 > pxL'
             && p.x + 10.0 < pxR'
      predS s = s.width >= one
             && s.point.x - s.width > pxL'
             && s.point.x + s.width < pxR'

  for_ (List.reverse ts) \segs -> do
    liftEff $ foreachE segs $ case _ of

      Left {drawing, points}  -> do
        let points' = filter predB points
        -- log $ "rendering " <> show (Array.length points') <> " glyphs, out of " <> show (Array.length points)
        renderBatch canvases.buffer {drawing, points: points'} trackCtx

      Right gs -> do
        let gs' = filter predS gs
        -- log $ "rendering " <> show (Array.length gs') <> " glyphs, out of " <> show (Array.length gs)
        foreachE gs' \s ->
          Drawing.render trackCtx
            $ Drawing.translate s.point.x s.point.y
            $ s.drawing

  void $ liftEff $ Canvas.translate { translateX: offset, translateY: zero } trackCtx



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

       let new = sofar <> m

       putVar new av
       delay (wrap 20.0)

       pure Nothing



type TrackVar a = AVar (Map ChrId (Array a))


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
  gwasProd   <- traverse (produceGWAS cs) urls.gwas
  annotsProd <- traverse (produceAnnots cs) urls.annotations
  genesProd  <- traverse (produceGenes cs) urls.genes

  gwas        <- makeVar mempty
  annotations <- makeVar mempty
  genes       <- makeVar mempty

  let gwasProc :: Maybe (Process (Aff _) Unit)
      gwasProc   = (_ $$ chunkConsumer gwas)   <$> gwasProd
      annotsProc = (_ $$ chunkConsumer annotations) <$> annotsProd
      genesProc  = (_ $$ chunkConsumer genes)  <$> genesProd

  _ <- traverse forkAff (runProcess <$> gwasProc)
  _ <- traverse forkAff (runProcess <$> annotsProc)
  _ <- traverse forkAff (runProcess <$> genesProc)

  pure { gwas, annotations, genes }


runBrowser :: Conf -> Eff _ _
runBrowser config = launchAff $ do

  {width} <- liftEff $ getScreenSize

  let height = config.browserHeight
      browserDimensions = {width, height}
      vScaleWidth = 60.0
      legendWidth = 120.0
      trackWidth = width - (vScaleWidth + legendWidth)

  bCanvas <- do
    doc <- liftEff $ DOM.htmlDocumentToDocument
           <$> (DOM.document =<< DOM.window)
    cont <- liftEff $ DOM.querySelector (wrap "#browser") (toParentNode doc)

    case cont of
      Nothing -> throwError $ error "Could not find browser element"
      Just el -> liftEff do
        createBrowserCanvas
          el browserDimensions
          { translateX: vScaleWidth
          , translateY: zero }


  trackData <- do
    genes <- traverse (getBedGenes cSys) (Just "./mouse.json")
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
    dragScroll trackWidth bCanvas viewCmds

  viewState <- makeVar { visible: initialView }

  renderFiber <- makeEmptyVar


  let
      entries = foldMap annotLegendTest trackData.annotations
      legend = { width: legendWidth, entries }
      s = config.score
      vscale = { width: vScaleWidth, color: black
               , min: s.min, max: s.max, sig: s.sig }
      tracks = demoTracksBed vscale trackData
      mainBrowser = browser cSys browserDimensions config.padding {legend, vscale} tracks

  _ <- forkAff $ renderLoop cSys mainBrowser trackWidth bCanvas { viewCmds, viewState, renderFiber }



  pure unit




-- TODO do this parsing better. good enough for now, but jesus.
--              do it applicative. semiring or at least semigroup
parseConfig :: Json -> Either String Conf
parseConfig j = do
  obj <- note "Provide a JSON object as configuration" $ j ^? _Object
  browserHeight <-
    note "`browserHeight` should be a Number" $ obj ^? ix "browserHeight" <<< _Number

  padding <- do
    p <- note "`padding` should be an object" $ obj ^? ix "padding" <<< _Object
    vertical <-
      note "`padding.vertical` should be a Number" $ p ^? ix "vertical" <<< _Number
    horizontal <-
      note "`padding.horizontal` should be a Number" $ p ^? ix "horizontal" <<< _Number
    pure {vertical, horizontal}

  score <- do
    s <- note "`score` should be an object" $ obj ^? ix "score" <<< _Object
    min <-
      note "`score.min` should be a Number" $ s ^? ix "min" <<< _Number
    max <-
      note "`score.max` should be a Number" $ s ^? ix "max" <<< _Number
    sig <-
      note "`score.sig` should be a Number" $ s ^? ix "sig" <<< _Number

    pure {min, max, sig}

  urls <- do
    u <- note "`urls` should be an object" $ obj ^? ix "urls" <<< _Object

    let gwas = u ^? ix "gwas" <<< _String
        annotations = u ^? ix "annotations" <<< _String
        genes = u ^? ix "genes" <<< _String

    pure {gwas, annotations, genes}

  pure {browserHeight, padding, score, urls}


initBrowser :: Json -> Eff _ _
initBrowser conf = do

  case parseConfig conf of
    Left err -> unsafeCrashWith err
    Right c  -> runBrowser c
