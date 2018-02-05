module Genetics.Browser.Track.UI where

import Prelude

import Color (black)
import Control.Monad.Aff (Aff, Fiber, forkAff, killFiber, launchAff, launchAff_)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, makeVar, putVar, takeVar, tryTakeVar)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
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
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Lens (to, (^.), (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(Nothing, Just), fromJust, fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Pair (Pair(..))
import Data.Ratio (Ratio, (%))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Variant (case_, onMatch)
import FRP.Event (Event)
import Genetics.Browser.Track.Backend (Glyph, Padding, browser, bumpFeatures, zipMapsWith)
import Genetics.Browser.Track.Demo (annotLegendTest, demoTracks, getAnnotations, getGWAS, getGenes)
import Genetics.Browser.Types (Bp(..), ChrId(ChrId), Point)
import Genetics.Browser.Types.Coordinates (BrowserPoint, CoordSys, Interval, _BrowserSize, mkCoordSys, shiftIntervalBy, zoomIntervalBy)
import Genetics.Browser.View (Pixels)
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

foreign import clearCanvas :: forall eff. CanvasElement -> Eff eff Unit


-- set an event to fire on the given button id

foreign import buttonEvent :: forall eff.
                              String
                           -> Eff eff Unit
                           -> Eff eff Unit


foreign import canvasEvent :: String -> CanvasElement -> Event Point


foreign import setViewUI :: forall eff. String -> Eff eff Unit


type ViewRange = Interval BrowserPoint

data UpdateView =
    ScrollView (Ratio BigInt)
  | ZoomView (Ratio BigInt)
  | ModView (ViewRange -> ViewRange)

updateViewFold :: UpdateView
               -> ViewRange
               -> ViewRange
updateViewFold uv iv@(Pair l r) = case uv of
  ZoomView   x -> iv `zoomIntervalBy`  x
  ScrollView x -> iv `shiftIntervalBy` x
  ModView f    -> f iv


queueCmd :: AVar UpdateView -> UpdateView -> Eff _ Unit
queueCmd av cmd = launchAff_ $ putVar cmd av


btnScroll :: Ratio BigInt -> AVar UpdateView -> Eff _ Unit
btnScroll x av = do
  buttonEvent "scrollLeft"  $ queueCmd av $ ScrollView (-x)
  buttonEvent "scrollRight" $ queueCmd av $ ScrollView   x


btnZoom :: Ratio BigInt -> AVar UpdateView -> Eff _ Unit
btnZoom x av = do
  buttonEvent "zoomOut" $ queueCmd av $ ZoomView   x
  buttonEvent "zoomIn"  $ queueCmd av $ ZoomView (-x)


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
      g x = let width' = BigInt.fromInt $ Int.round width
                x'     = BigInt.fromInt $ Int.round x
            in x' % width'
  in canvasDrag f cnv.overlay



foreign import canvasWheelEvent :: CanvasElement -> Event Number

scrollZoomEvent :: CanvasElement -> Event UpdateView
scrollZoomEvent el = map (ZoomView <<< f) $ canvasWheelEvent el
  where f :: Number -> Ratio BigInt
        f dY = let d' = 10000.0
                   n = BigInt.fromInt $ Int.round $ dY * d'
                   d = BigInt.fromInt $ Int.round $ d' * 100.0
               in n % d


-- TODO differentiate scroll buffer & rendering buffer
type BrowserCanvas = { buffer  :: CanvasElement
                     , track   :: CanvasElement
                     , overlay :: CanvasElement
                     }

createBrowserCanvas :: Element
                    -> { width :: Number, height :: Number }
                    -> Eff _ BrowserCanvas
createBrowserCanvas el dim = do
  let node :: CanvasElement -> Node
      node = unsafeCoerce
      element :: CanvasElement -> Element
      element = unsafeCoerce

  buffer  <- newCanvas dim
  track   <- newCanvas dim
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


coordSys :: CoordSys ChrId BrowserPoint
coordSys = mkCoordSys mouseChrSizes (BigInt.fromInt 2000000)

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


type Conf = { browserHeight :: Pixels
            , padding :: Padding
            , score :: { min :: Number, max :: Number, sig :: Number }
            , urls :: { gwas        :: Maybe String
                      , annotations :: Maybe String
                      , genes       :: Maybe String
                      }
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


browserLoop :: { tracks     :: ViewRange -> Array Glyph
               , relativeUI :: ViewRange -> Drawing
               , fixedUI :: Drawing }
            -> BrowserCanvas
            -> { viewState   :: AVar BrowserState
               , viewCmds    :: AVar UpdateView
               , renderFiber :: AVar (Fiber _ _)}
            -> Aff _ _
browserLoop browser canvases state = forever do

  vState <- takeVar state.viewState

  traverse_ (killFiber (error "Resetting renderer"))
    =<< tryTakeVar state.renderFiber

  renderer <- forkAff $ renderGlyphs (browser.tracks vState.visible) canvases

  liftEff $ do
    trackCtx   <- getContext2D canvases.track
    overlayCtx <- getContext2D canvases.overlay
    Drawing.render trackCtx $ browser.relativeUI vState.visible
    Drawing.render overlayCtx browser.fixedUI

  -- wait until UI has been clicked, view scrolled, etc.
  cmd <- takeVar state.viewCmds

  let newState = updateViewFold cmd vState.visible

  putVar { visible: newState } state.viewState

  pure unit


renderGlyphs :: Array Glyph
             -> BrowserCanvas
             -> Aff _ Unit
renderGlyphs gs canvases = do
  {width, height} <- liftEff $ Canvas.getCanvasDimensions canvases.track
  let bg = filled (fillColor white) $ rectangle 0.0 0.0 width height

  trackCtx   <- liftEff $ getContext2D canvases.track
  overlayCtx <- liftEff $ getContext2D canvases.overlay

  liftEff $ Drawing.render trackCtx bg
  liftEff $ foreachE gs $ case_ # onMatch
    { batched: (\t -> renderBatch canvases.buffer t trackCtx)
    , single: (\s -> Drawing.render trackCtx
                       $ Drawing.translate s.point.x s.point.y
                       $ s.drawing) }


runBrowser :: Conf -> Eff _ _
runBrowser config = launchAff $ do

  {width} <- liftEff $ getScreenSize

  let height = config.browserHeight
      browserDimensions = {width, height}
      vScaleWidth = 40.0
      legendWidth = 100.0
      trackWidth = width - (vScaleWidth + legendWidth)

  bCanvas <- do
    doc <- liftEff $ DOM.htmlDocumentToDocument
           <$> (DOM.document =<< DOM.window)
    cont <- liftEff $ DOM.querySelector (wrap "#browser") (toParentNode doc)

    maybe
      (throwError $ error "Could not find browser element")
      (\el -> liftEff $ createBrowserCanvas el browserDimensions)
      cont


  trackData <- do
    gwas  <- traverse (getGWAS  coordSys) config.urls.gwas
    genes <- traverse (getGenes coordSys) config.urls.genes
    rawAnnotations <-
      traverse (getAnnotations coordSys) config.urls.annotations

    let annotations = zipMapsWith
                       (bumpFeatures (to _.score) (SProxy :: SProxy "score")
                         (Bp 1000000.0))
                       <$> gwas <*> rawAnnotations

    pure { genes, gwas, annotations }



  let initialView :: Interval BrowserPoint
      initialView = Pair zero (coordSys^._BrowserSize)

  viewCmds <- makeEmptyVar
  liftEff $ do
    btnScroll (one % BigInt.fromInt 20) viewCmds
    btnZoom   (one % BigInt.fromInt 20) viewCmds
    dragScroll trackWidth bCanvas viewCmds

  viewState <- makeVar { visible: initialView }

  renderFiber <- makeEmptyVar


  let
      entries = foldMap annotLegendTest trackData.annotations
      legend = { width: legendWidth, entries }
      s = config.score
      vscale = { width: vScaleWidth, color: black
               , min: s.min, max: s.max, sig: s.sig }
      tracks = demoTracks vscale trackData
      mainBrowser = browser coordSys browserDimensions config.padding {legend, vscale} tracks

  browserLoop mainBrowser bCanvas { viewCmds, viewState, renderFiber }




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
