module Genetics.Browser.Track.UI where

import Prelude

import Color (black)
import Control.Alt ((<|>))
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff, forE, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Uncurried (EffFn4, runEffFn4)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Error.Class (throwError)
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
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Filterable (filterMap)
import Data.Foldable (foldMap, length, sum)
import Data.Int as Int
import Data.Lens (_Left, to, (^.), (^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing, Just), fromJust, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Pair (Pair(..))
import Data.Ratio (Ratio, (%))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(Tuple))
import FRP.Event (Event)
import FRP.Event as Event
import FRP.Event as FRP
import Genetics.Browser.Track.Backend (Padding, Renderer, bumpFeatures, renderBatchTrack, renderTrack, zipMapsWith)
import Genetics.Browser.Track.Demo (annotLegendTest, basicRenderers, demoBatchBrowser, demoBrowser, getAnnotations, getGWAS, getGenes)
import Genetics.Browser.Types (Bp(..), ChrId(ChrId), Point)
import Genetics.Browser.Types.Coordinates (BrowserPoint, CoordInterval, CoordSys, Interval, RelPoint, _BrowserSize, canvasToView, findBrowserInterval, intervalToGlobal, mkCoordSys, shiftIntervalBy, zoomIntervalBy)
import Genetics.Browser.View (Pixels)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, Context2D, getContext2D)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, white)
import Graphics.Drawing as Drawing
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Performance.Minibench (bench)
import Unsafe.Coerce (unsafeCoerce)



foreign import getScreenSize :: forall eff. Eff eff { width :: Number, height :: Number }

-- 1st element is a backbuffer, 2nd the one shown on screen
foreign import scrollCanvas :: forall eff.
                               CanvasElement
                            -> CanvasElement
                            -> Point
                            -> Eff eff Unit

foreign import canvasDragImpl :: CanvasElement -> Event { during :: Nullable Point
                                                        , total :: Nullable Point }

-- creates a new CanvasElement, not attached to the DOM and thus not visible
foreign import newCanvas :: forall eff.
                            { width :: Number, height :: Number }
                         -> Eff eff CanvasElement

foreign import clearCanvas :: forall eff. CanvasElement -> Eff eff Unit


-- set an event to fire on the given button id
foreign import buttonEvent :: String
                           -> Event Unit


foreign import canvasEvent :: String -> CanvasElement -> Event Point


foreign import setViewUI :: forall eff. String -> Eff eff Unit

type BrowserView = Interval BrowserPoint

data UpdateView =
    ScrollView (Ratio BigInt)
  | ZoomView (Ratio BigInt)
  | ModView (BrowserView -> BrowserView)


btnScroll :: forall r.
             Ratio BigInt
          -> Event UpdateView
btnScroll x =  const (ScrollView (-x)) <$> buttonEvent "scrollLeft"
           <|> const (ScrollView   x ) <$> buttonEvent "scrollRight"

btnZoom :: (Ratio BigInt)
        -> Event UpdateView
btnZoom m =  const (ZoomView    m ) <$> buttonEvent "zoomOut"
         <|> const (ZoomView  (-m)) <$> buttonEvent "zoomIn"

btnUpdateView :: { scroll :: Ratio BigInt
                 , zoom :: Ratio BigInt
                 , reset :: BrowserView }
              -> Event UpdateView
btnUpdateView {scroll, zoom, reset}
  =  btnScroll scroll
 <|> btnZoom zoom
 <|> (const (ModView (const reset)) <$> buttonEvent "reset")


updateViewFold :: CoordSys ChrId BrowserPoint
               -> UpdateView
               -> BrowserView
               -> BrowserView
updateViewFold cs uv iv@(Pair l r) = case uv of
  ZoomView   x -> iv `zoomIntervalBy`  x
  ScrollView x -> iv `shiftIntervalBy` x
  ModView f    -> f iv


browserViewEvent :: CoordSys ChrId BrowserPoint
                 -> BrowserView
                 -> Event UpdateView
                 -> Event BrowserView
browserViewEvent cs start ev =
  Event.fold
    (\a b -> (normalizeView start) (updateViewFold cs a b)) ev start
  where normalizeView (Pair lhs rhs) (Pair l r)
          = Pair (max lhs l) (min r rhs)



browserBatchDrawEvent :: CoordSys ChrId BrowserPoint
                      -> Canvas.Dimensions
                      -> Padding
                      -> { min :: Number, max :: Number, sig :: Number }
                      -> { vScaleWidth :: Pixels, legendWidth :: Pixels }
                      -> { gwas        :: Maybe (Map ChrId (List _))
                         , annotations :: Maybe (Map ChrId (List _))
                         , genes       :: Maybe (Map ChrId (List _)) }
                      -> Event BrowserView
                      -> Event {track :: Array _, overlay :: Drawing}
browserBatchDrawEvent csys cdim padding {min,max,sig} uiSize dat
  = let
        entries = foldMap annotLegendTest dat.annotations
        legend = { width: uiSize.legendWidth, entries }

        vscale = { width: uiSize.vScaleWidth, color: black, min, max, sig }

        dd = demoBatchBrowser csys cdim padding {legend, vscale} dat

    in map dd


clickEvent :: forall r. CanvasElement -> Event Pixels
clickEvent el = (_.x) <$> canvasEvent "mousedown" el


globalClick :: Event (Interval BrowserPoint) -> Event (Ratio BigInt) -> Event BrowserPoint
globalClick vs vx = (\iv r -> intervalToGlobal iv r) <$> vs <*> vx


chrClick :: CoordSys ChrId BrowserPoint
         -> Event (Maybe (CoordInterval _ _))
         -> Event (Maybe (Tuple ChrId Bp))
chrClick csys ev = map (\ {chrSize, index}
                       -> Tuple index chrSize) <$> ev


showView :: Interval BrowserPoint -> String
showView (Pair l r) =
     "< "
  <> BigInt.toString (unwrap l)
  <> " -- "
  <> BigInt.toString (unwrap r)
  <> " >"

showLP :: Interval BrowserPoint -> RelPoint -> String
showLP iv p =
     "Interval: "
  <> showView iv
  <> ";\t"
  <> show p


canvasDrag :: CanvasElement -> Event (Either Point Point)
canvasDrag el = f <$> canvasDragImpl el
  where f ev = case toMaybe ev.during of
          Just p  -> Right p
          Nothing -> Left $ fromMaybe {x:zero,y:zero} (toMaybe ev.total)


browserDrag :: forall r.
               { width :: Number | r }
            -> Event Point
            -> Event (Ratio BigInt)
browserDrag w ev = f <$> ev
  where f :: _ -> Ratio BigInt
        f {x} = let width' = BigInt.fromInt $ Int.round w.width
                    x'     = BigInt.fromInt $ Int.round x
                in x' % width'


foreign import canvasWheelEvent :: CanvasElement -> Event Number

scrollZoomEvent :: CanvasElement -> Event UpdateView
scrollZoomEvent el = map (ZoomView <<< f) $ canvasWheelEvent el
  where f :: Number -> Ratio BigInt
        f dY = let d' = 10000.0
                   n = BigInt.fromInt $ Int.round $ dY * d'
                   d = BigInt.fromInt $ Int.round $ d' * 100.0
               in n % d




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
            [ Tuple (ChrId "1")   (unsafePartial $ fromJust $ BigInt.fromString "195471971")
            , Tuple (ChrId "2")   (unsafePartial $ fromJust $ BigInt.fromString "182113224")
            , Tuple (ChrId "3")   (unsafePartial $ fromJust $ BigInt.fromString "160039680")
            , Tuple (ChrId "4")   (unsafePartial $ fromJust $ BigInt.fromString "156508116")
            , Tuple (ChrId "5")   (unsafePartial $ fromJust $ BigInt.fromString "151834684")
            , Tuple (ChrId "6")   (unsafePartial $ fromJust $ BigInt.fromString "149736546")
            , Tuple (ChrId "7")   (unsafePartial $ fromJust $ BigInt.fromString "145441459")
            , Tuple (ChrId "8")   (unsafePartial $ fromJust $ BigInt.fromString "129401213")
            , Tuple (ChrId "9")   (unsafePartial $ fromJust $ BigInt.fromString "124595110")
            , Tuple (ChrId "10")  (unsafePartial $ fromJust $ BigInt.fromString "130694993")
            , Tuple (ChrId "11")  (unsafePartial $ fromJust $ BigInt.fromString "122082543")
            , Tuple (ChrId "12")  (unsafePartial $ fromJust $ BigInt.fromString "120129022")
            , Tuple (ChrId "13")  (unsafePartial $ fromJust $ BigInt.fromString "120421639")
            , Tuple (ChrId "14")  (unsafePartial $ fromJust $ BigInt.fromString "124902244")
            , Tuple (ChrId "15")  (unsafePartial $ fromJust $ BigInt.fromString "104043685")
            , Tuple (ChrId "16")  (unsafePartial $ fromJust $ BigInt.fromString "98207768")
            , Tuple (ChrId "17")  (unsafePartial $ fromJust $ BigInt.fromString "94987271")
            , Tuple (ChrId "18")  (unsafePartial $ fromJust $ BigInt.fromString "90702639")
            , Tuple (ChrId "19")  (unsafePartial $ fromJust $ BigInt.fromString "61431566")
            , Tuple (ChrId "X")   (unsafePartial $ fromJust $ BigInt.fromString "17103129")
            , Tuple (ChrId "Y")   (unsafePartial $ fromJust $ BigInt.fromString "9174469")
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
    case cont of
      Nothing -> throwError $ error "Could not find browser element"
      Just el -> liftEff $ createBrowserCanvas el browserDimensions


  let browserDragEvent :: Event (Ratio BigInt)
      browserDragEvent = map negate
                         $ browserDrag {width: trackWidth}
                         $ filterMap (_^?_Left) (canvasDrag bCanvas.overlay)


  void $ liftEff $ unsafeCoerceEff $ FRP.subscribe (canvasDrag bCanvas.overlay) $ case _ of
    Left _      -> pure unit
    Right {x,y} -> scrollCanvas bCanvas.buffer bCanvas.track {x: -x, y: 0.0}


  updateBrowser <- liftEff $ Event.create


  let
      initialView :: Interval BrowserPoint
      initialView = Pair zero (coordSys^._BrowserSize)

      viewEvent :: Event BrowserView
      viewEvent = browserViewEvent coordSys initialView
                  $  btnUpdateView { scroll: one % BigInt.fromInt 20
                                   , zoom:   one % BigInt.fromInt 20
                                   , reset: initialView
                                   }
                 <|> map ScrollView browserDragEvent
                 <|> const (ModView id) <$> updateBrowser.event
                 <|> scrollZoomEvent bCanvas.overlay

      click = clickEvent bCanvas.overlay
      vClick = map (canvasToView {width}) click
      gClick = globalClick viewEvent vClick
      fClick = map (findBrowserInterval coordSys) gClick

      clickEvs :: Event { view :: BrowserView
                        , canvasClick :: Number
                        , vClick :: Ratio BigInt
                        , gClick :: BrowserPoint
                        , fClick :: Maybe (CoordInterval ChrId BrowserPoint)
                        , cClick :: Maybe (Tuple ChrId Bp) }
      clickEvs = { view: _
                 , canvasClick: _
                 , vClick: _
                 , gClick: _
                 , fClick: _
                 , cClick: _
                 }
                 <$> viewEvent
                 <*> click
                 <*> vClick
                 <*> gClick
                 <*> fClick
                 <*> chrClick coordSys fClick




  void $ liftEff $ Event.subscribe clickEvs
       \ev -> do
           let sF (Just x) = "Frame: " <> show (unsafeStringify x)
               sF Nothing = "No Frame"
               sChr x = "Chr: " <> unsafeStringify x
               -- sChr (Just x) = "Chr: " <> show chr <> "\t" <> show (unwrap bp)
               -- sChr Nothing = "No Chr"

           setViewUI $ "<p>" <> showView ev.view <> "</p>"
                    <> "<p>" <> "Canvas click:" <> show ev.canvasClick <> "</p>"
                    <> "<p>" <> "View click:" <> show ev.vClick <> "</p>"
                    <> "<p>" <> "Global click:" <> show (unwrap ev.gClick) <> "</p>"
                    <> "<p>Frame click: " <> sF ev.fClick <> "</p>"
                    <> "<p>Chr click: " <> sChr ev.cClick <> "</p>"


  trackData <- do
    gwas  <- traverse (getGWAS  coordSys) config.urls.gwas
    genes <- traverse (getGenes coordSys) config.urls.genes
    rawAnnotations <-
      traverse (getAnnotations coordSys) config.urls.annotations

    let annotations = zipMapsWith
                       (bumpFeatures (to _.score) (SProxy :: SProxy "score")
                         (Bp 1000000.0))
                       <$> gwas <*> rawAnnotations

    liftEff $ updateBrowser.push unit

    pure { genes, gwas, annotations }


  let ev' = browserBatchDrawEvent
              coordSys
              browserDimensions
              config.padding
              config.score
              {vScaleWidth, legendWidth}
              trackData
              viewEvent

      bg = filled (fillColor white) $ rectangle 0.0 0.0 width height


  -- -- TODO correctly render the layers
  trackCtx <- liftEff $ getContext2D bCanvas.track
  overlayCtx <- liftEff $ getContext2D bCanvas.overlay
  void $ liftEff $ Event.subscribe ev' \d -> do
    Drawing.render trackCtx bg
    foreachE d.track \t ->
      renderBatch bCanvas.buffer t trackCtx
    Drawing.render overlayCtx d.overlay


  liftEff $ updateBrowser.push unit



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
