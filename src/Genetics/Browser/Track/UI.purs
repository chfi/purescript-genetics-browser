module Genetics.Browser.Track.UI where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (empty, (<*>))
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (makeVar, putVar, tryPeekVar)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Error.Class (throwError)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array (take, zip)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Filterable (class Filterable, filter, filterMap)
import Data.Filterable as Filterable
import Data.Foldable (class Foldable, foldMap, for_, length, maximum, maximumBy, sum)
import Data.Int (round)
import Data.Int as Int
import Data.Lens (_Left, view, (^?))
import Data.Lens.Index (ix)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Monoid (mempty)
import Data.Newtype (ala, alaF, over, unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Ord.Max (Max(..))
import Data.Ratio (Ratio, (%))
import Data.Ratio as Ratio
import Data.Semigroup.Foldable (foldMap1)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Debug.Trace (traceShow)
import Debug.Trace as Debug
import FRP.Event (Event)
import FRP.Event as Event
import FRP.Event as FRP
import Genetics.Browser.Track.Backend (GWASFeature, Gene, drawDemo, drawDemo', getDataDemo, mouseChrIds)
import Genetics.Browser.Types (Bp(..), BrowserPoint(..), ChrId(..), CoordSys(..), Interval(..), IntervalPoint, LocalPoint(..), Point, canvasToBrowserOffset, canvasToView, frameToChr, globalToFrame, intervalSize, localToGlobal, mkCoordSys, shiftIntervalBy)
import Genetics.Browser.View (Pixels)
import Global as Global
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, Context2D, Transform, TranslateTransform, getCanvasElementById, getCanvasHeight, getContext2D, setCanvasWidth, transform, translate, withContext)
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, white)
import Graphics.Drawing as Drawing
import Math as Math
import Network.HTTP.Affjax as Affjax
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

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
    Scroll BigInt
  | Zoom (Ratio BigInt)
  | ScrollView (Ratio BigInt)
  | NoOp



btnScroll :: forall r.
             BigInt
          -> Event UpdateView
          --    BrowserPoint
          -- -> BrowserPoint
          -- -> Event UpdateView
btnScroll x = const (Scroll x) <$> buttonEvent "scrollLeft" <|>
              const (Scroll (negate x)) <$> buttonEvent "scrollRight"
-- btnScroll s (BPoint x) = scrollView s (wrap x)  <$> buttonEvent "scrollLeft"
                         -- const (scrollView s (wrap $ negate x)) <$> buttonEvent "scrollRight"

btnZoom :: (Ratio BigInt)
        -> Event UpdateView
btnZoom m = const (Zoom (m )) <$> buttonEvent "zoomOut" <|>
            const (Zoom (m * (BigInt.fromInt 9 % BigInt.fromInt 100))) <$> buttonEvent "zoomIn"

btnUpdateView :: {scroll :: BigInt, zoom :: Ratio BigInt } -> Event UpdateView
-- btnUpdateView {scroll,zoom} = btnScroll scroll <|> btnZoom zoom <|> (const NoOp <$> buttonEvent "redraw")
btnUpdateView  {scroll, zoom } = btnScroll scroll <|>
                                 btnZoom zoom <|>
                                 (const NoOp <$> buttonEvent "redraw")
-- btnUpdateView = const NoOp <$> buttonEvent "redraw"

scrollView :: BrowserPoint
           -> BigInt
           -> BrowserView
           -> BrowserView
scrollView (BPoint size) p (Interval (BPoint l) (BPoint r)) =
  let p' = p
  in Interval (BPoint $ max zero (l - p')) (BPoint $ min size (r - p'))


scrollViewGood :: Ratio BigInt
               -> BrowserView
               -> BrowserView
scrollViewGood rat iv@(Interval (BPoint l) (BPoint r)) =
  iv `shiftIntervalBy` rat


zoomView :: BrowserPoint
         -> Ratio BigInt
         -> BrowserView
         -> BrowserView
zoomView (BPoint size) p iv@(Interval (BPoint l) (BPoint r)) =
  let len = r - l
      len' = (len * (Ratio.numerator p)) / (Ratio.denominator p)
  in (Interval (BPoint $ max zero (l + len')) (BPoint $ min size (r - len')))



normalizeView :: BrowserView
              -> BrowserView
              -> BrowserView
normalizeView (Interval (BPoint lhs) (BPoint rhs))
              (Interval (BPoint l)   (BPoint r)) = Interval (BPoint $ max lhs l) (BPoint $ min r rhs)


updateViewFold :: { size :: _, scroll :: _, zoom :: _ }
               -> UpdateView
               -> BrowserView
               -> BrowserView
-- updateViewFold _ iv = iv
updateViewFold ssz uv iv@(Interval l r) = case uv of
  Scroll p  -> scrollView ssz.size p iv
  Zoom r    -> zoomView ssz.size r iv
  ScrollView x -> scrollViewGood x iv
  _ -> iv

browserViewEvent :: { size :: BrowserPoint, scroll :: _, zoom :: _ }
                 -> BrowserView
                 -> Event UpdateView
                 -> Event BrowserView
browserViewEvent ssz start ev = Event.fold (\a b -> (normalizeView start) (updateViewFold ssz a b)) ev start


drawingEvent :: { min :: Number, max :: Number }
             -> CoordSys ChrId BrowserPoint
             -> { width :: Pixels, height :: Pixels, yOffset :: Pixels }
             -> { gwas  :: Map ChrId (List _)
                , annots :: Map ChrId (List _) }
             -> Event BrowserView
             -> Event Drawing
drawingEvent s csys box dat = let dd = drawDemo' csys s 0.25 box dat
                              in map dd


clickEvent :: forall r. CanvasElement -> Event Pixels
clickEvent el = (_.x) <$> canvasEvent "mousedown" el


viewClick :: forall r. { width :: Pixels | r} -> Event Pixels -> Event (Ratio BigInt)
viewClick w = map (canvasToView w)


globalClick :: Event (Interval BrowserPoint) -> Event (Ratio BigInt) -> Event BrowserPoint
globalClick vs vx = (\iv r -> localToGlobal $ Local iv r) <$> vs <*> vx


frameClick :: CoordSys ChrId BrowserPoint -> Event BrowserPoint -> Event (Maybe (Tuple ChrId IntervalPoint))
frameClick csys = map (globalToFrame csys)

chrClick :: CoordSys ChrId BrowserPoint
         -> Event (Maybe (Tuple ChrId IntervalPoint))
         -> Event (Maybe (Tuple ChrId Bp))
chrClick csys ev = f <$> ev
  where f x = do
          p@(Tuple i _) <- x
          Tuple i <$> frameToChr csys p


showView :: Interval BrowserPoint -> String
showView (Interval (BPoint l) (BPoint r)) = "< " <> BigInt.toString l <> " -- " <> BigInt.toString r <> " >"

showLP :: IntervalPoint -> String
showLP (Local iv p) = "Interval: " <> showView iv <> ";\t" <> show p


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
        f {x} = let width = BigInt.fromInt $ Int.round w.width
                    x' = BigInt.fromInt $ Int.round x
                in x' % width


scrollViewEvent :: Event (Ratio BigInt)
                -> Event UpdateView
scrollViewEvent = map ScrollView



  -- let cDrag = canvasDrag canvas
  --     lefts = case _ of
  --       Left x -> Just x
  --       Right _ -> Nothing
  --     -- the alt operator <|> combines the two event streams,
  --     -- resulting in an event of both button-click scrolls
  --     -- and canvas-drag scrolls

      -- dragScroll = filterMap (map (ScrollPixels <<< _.x) <<< lefts) cDrag
      -- updateViews = btnScroll (Bp 500.0)
      --           <|> dragScroll
      --           <|> btnZoom (wrap <<< (_ * 0.8) <<< unwrap)
      --                       (wrap <<< (_ * 1.2) <<< unwrap)
      --           <|> scrollZoom canvas
      -- viewB = FRP.fold foldView updateViews v


  -- _ <- liftEff $ unsafeCoerceEff $ FRP.subscribe cDrag $ case _ of
  --   Left _      -> pure unit
  --   Right {x,y} -> scrollCanvas backCanvas canvas {x: -x, y: 0.0}



main :: Eff _ _
main = launchAff do

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

      chrIds = mouseChrIds


  let dragCanvasEv :: _
      dragCanvasEv = canvasDrag canvas

      browserDragEv :: Event (Ratio BigInt)
      browserDragEv = browserDrag {width: w} (filterMap (_^?_Left) dragCanvasEv)

      dregs :: _
      dregs = scrollViewEvent browserDragEv


  let cs@(CoordSys s) = coordSys

      scroll :: BigInt
      scroll = BigInt.fromInt 10000000

      zoom :: Ratio BigInt
      zoom = one % (BigInt.fromInt 1000)


  let
      begin :: Interval BrowserPoint
      -- begin = Interval (BPoint $ BigInt.fromInt 652428823) (BPoint $ BigInt.fromInt 1829028329)
      begin = Interval (BPoint $ BigInt.fromInt 652428823) s.size

      viewEvent :: Event (Interval BrowserPoint)
      viewEvent = browserViewEvent {size: s.size, scroll, zoom } begin
                    $ btnUpdateView {scroll: scroll, zoom } <|> dregs



      click :: Event Pixels
      click = clickEvent canvas

      vClick :: Event (Ratio BigInt)
      vClick = viewClick {width: w} click

      gClick :: Event BrowserPoint
      gClick = globalClick viewEvent vClick

      fClick :: Event (Maybe _)
      fClick = frameClick cs gClick

      cClick :: Event (Maybe _)
      cClick = chrClick cs fClick

      clickEvs :: Event { view :: _
                        , canvasClick :: _
                        , vClick :: _
                        , gClick :: _
                        , fClick :: _
                        , cClick :: _ }
      clickEvs = { view: _, canvasClick: _, vClick: _
                 , gClick: _, fClick: _, cClick: _ }
                 <$> viewEvent <*> click <*> vClick
                 <*> gClick <*> fClick <*> cClick


  void $ liftEff $ unsafeCoerceEff $ FRP.subscribe dragCanvasEv $ case _ of
    Left _      -> pure unit
    Right {x,y} -> scrollCanvas backCanvas canvas {x: -x, y: 0.0}


  void $ liftEff $ Event.subscribe clickEvs
       (\ev -> do
           let sF (Just (Tuple chr i)) = "Frame: " <> show chr <> showLP i
               sF Nothing = "No Frame"
               sChr (Just (Tuple chr bp)) = "Chr: " <> show chr <> "\t" <> show (unwrap bp)
               sChr Nothing = "No Chr"

           setViewUI $ "<p>" <> showView ev.view <> "</p>"
                    <> "<p>" <> "Canvas click:" <> show ev.canvasClick <> "</p>"
                    <> "<p>" <> "View click:" <> show ev.vClick <> "</p>"
                    <> "<p>" <> "Global click:" <> show (unwrap ev.gClick) <> "</p>"
                    <> "<p>Frame click: " <> sF ev.fClick <> "</p>"
                    <> "<p>Chr click: " <> sChr ev.cClick <> "</p>"
           )


  dat <- getDataDemo { gwas: "./gwas.json"
                     , annots: "./annots_fake.json" }

  let sizes = {width: w, height: h, yOffset: 5.0}
      score = {min: 0.125, max: 0.42}

  let ev' = drawingEvent score cs sizes dat viewEvent
      bg = filled (fillColor white) $ rectangle 0.0 0.0 w h

  void $ liftEff $ Event.subscribe ev' (\d -> Drawing.render ctx (bg <> d))



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
