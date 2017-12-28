module Genetics.Browser.Track.UI
       ( main
       ) where

import Prelude

import Color.Scheme.Clrs (red)
import Control.Alt ((<|>))
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Int as Int
import Data.Lens (_Left, (^?), (^.))
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(Nothing, Just), fromJust)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Pair (Pair(..))
import Data.Ratio (Ratio, (%))
import Data.Tuple (Tuple(Tuple))
import FRP.Event (Event)
import FRP.Event as Event
import FRP.Event as FRP
import Genetics.Browser.Track.Backend (demoBrowser, demoLegend, drawDemo, getDataDemo)
import Genetics.Browser.Types (Bp, ChrId(ChrId), Point)
import Genetics.Browser.Types.Coordinates (BrowserPoint, CoordInterval, CoordSys, Interval, RelPoint, _BrowserSize, canvasToView, findBrowserInterval, intervalToGlobal, mkCoordSys, shiftIntervalBy, zoomIntervalBy)
import Genetics.Browser.View (Pixels)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, getCanvasElementById, getCanvasHeight, getContext2D, setCanvasWidth)
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, white)
import Graphics.Drawing as Drawing
import Partial.Unsafe (unsafePartial)


foreign import getScreenSize :: forall eff. Eff eff { w :: Number, h :: Number }

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
                            { w :: Number, h :: Number }
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


browserDrawEvent :: CoordSys ChrId BrowserPoint
                 -> { width :: Pixels, height :: Pixels }
                 -> { min :: Number, max :: Number, sig :: Number }
                 -- -> { vScaleWidth :: Pixels, legendWidth :: Pixels }
                 -> { gwas   :: Map ChrId (List _)
                    , annots :: Map ChrId (List _) }
                 -> Event BrowserView
                 -> Event Drawing
browserDrawEvent csys canvasSize vscale dat
  = let dd = demoBrowser csys canvasSize vscale {vScaleWidth, legendWidth} red demoLegend dat
        vScaleWidth = 40.0
        legendWidth = 100.0
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
          Nothing -> Left $ unsafePartial $ fromJust (toMaybe ev.total)


browserDrag :: forall r.
               { width :: Number | r }
            -> Event Point
            -> Event (Ratio BigInt)
browserDrag w ev = f <$> ev
  where f :: _ -> _
        f {x} = let width' = BigInt.fromInt $ Int.round w.width
                    x' = BigInt.fromInt $ Int.round x
                in x' % width'


foreign import canvasWheelEvent :: CanvasElement -> Event Number

scrollZoomEvent :: CanvasElement -> Event UpdateView
scrollZoomEvent el = map (ZoomView <<< f) $ canvasWheelEvent el
  where f :: Number -> Ratio BigInt
        f dY = let d' = 10000.0
                   n = BigInt.fromInt $ Int.round $ dY * d'
                   d = BigInt.fromInt $ Int.round $ 10000.0
               in n % d



main :: Eff _ _
main = launchAff $ do

  canvas <- liftEff $ unsafePartial $ fromJust <$> getCanvasElementById "canvas"
  ctx <- liftEff $ getContext2D canvas

  {w,h} <- liftEff do
    {w} <- getScreenSize
    h <- getCanvasHeight canvas
    _ <- setCanvasWidth (w-2.0) canvas
    pure {w, h}

  backCanvas <- liftEff $ newCanvas {w,h}

  let height = h
      offset = 0.0
      yInfo = { height, offset }

      browserDragEvent :: Event (Ratio BigInt)
      browserDragEvent = map negate
                         $ browserDrag {width: w}
                         $ filterMap (_^?_Left) (canvasDrag canvas)



  updateBrowser <- liftEff $ Event.create

  let
      begin :: Interval BrowserPoint
      begin = Pair zero (coordSys^._BrowserSize)

      viewEvent :: Event BrowserView
      viewEvent = browserViewEvent coordSys begin
                  $  btnUpdateView { scroll: one % BigInt.fromInt 20
                                   , zoom:   one % BigInt.fromInt 20
                                   , reset: begin
                                   }
                 <|> map ScrollView browserDragEvent
                 <|> const (ModView id) <$> updateBrowser.event
                 <|> scrollZoomEvent canvas

      click = clickEvent canvas
      vClick = map (canvasToView {width: w}) click
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
                 <*> clickEvent canvas
                 <*> vClick
                 <*> gClick
                 <*> fClick
                 <*> chrClick coordSys fClick


  void $ liftEff $ unsafeCoerceEff $ FRP.subscribe (canvasDrag canvas) $ case _ of
    Left _      -> pure unit
    Right {x,y} -> scrollCanvas backCanvas canvas {x: -x, y: 0.0}


  void $ liftEff $ Event.subscribe clickEvs
       (\ev -> do
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
           )

  dat <- do
    res <- getDataDemo { gwas: "./gwas.json"
                       , annots: "./annots_fake.json" }
    liftEff $ updateBrowser.push unit
    pure res

  let browserSize = {width: w, height: h}
      score = {min: 0.125, max: 0.42, sig: 0.25}

  let ev' = browserDrawEvent coordSys browserSize score dat viewEvent
      bg = filled (fillColor white) $ rectangle 0.0 0.0 w h

  void $ liftEff $ Event.subscribe ev' (\d -> Drawing.render ctx (bg <> d))


  liftEff $ updateBrowser.push unit


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
