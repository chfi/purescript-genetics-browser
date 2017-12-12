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
import Data.Lens (view, (^?))
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
import FRP.Event (Event)
import FRP.Event as Event
import FRP.Event as FRP
import Genetics.Browser.Track.Backend (GWASFeature, Gene, drawDemo, getDataDemo, mouseChrIds)
import Genetics.Browser.Types (Bp(..), BrowserPoint(..), ChrId(..), CoordSys(..), Interval(..), IntervalPoint, LocalPoint(..), Point, canvasToBrowserOffset, canvasToView, frameToChr, globalToFrame, intervalSize, intervalToGlobal, mkCoordSys)
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


-- type View = { lHand :: ChrId, rHand :: ChrId }

-- type ZipperRange a =
--   { left  :: List a
--   , focus :: List a
--   , right :: List a
--   }

-- zipSlideLeft :: forall a.
--                 ZipperRange a
--              -> ZipperRange a
-- zipSlideLeft zr@{left, focus, right} = case List.uncons left, List.unsnoc focus of
--     Nothing, _  -> zr
--     _ , Nothing -> zr
--     Just l, Just f ->
--       let left'  = l.tail
--           focus' = l.head : f.init
--           right' = f.last : right
--       in {left: left', focus: focus', right: right'}

-- zipSlideRight :: forall a.
--                  ZipperRange a
--               -> ZipperRange a
-- zipSlideRight zr@{left, focus, right} = case List.uncons focus, List.uncons right of
--     Nothing, _  -> zr
--     _ , Nothing -> zr
--     Just f, Just r ->
--       let left'  = f.head : left
--           focus' = f.tail <> List.singleton r.head
--           right' = r.tail
--       in {left: left', focus: focus', right: right'}


-- zipZoomIn :: forall a.
--              ZipperRange a
--           -> ZipperRange a
-- zipZoomIn zr@{left, focus, right} = case List.uncons focus of
--   Nothing -> zr
--   Just {head, tail} -> case List.unsnoc tail of
--     Nothing -> zr
--     Just {init, last} ->
--       let left'  = head : left
--           focus' = init
--           right' = last : right
--       in {left: left', focus: focus', right: right'}


-- zipZoomOut :: forall a.
--               ZipperRange a
--            -> ZipperRange a
-- zipZoomOut zr@{left, focus, right} = case List.uncons left, List.uncons right of
--   Nothing, Nothing -> zr
--   Nothing, Just r ->
--     let focus' = focus <> List.singleton r.head
--         right' = r.tail
--     in zr { focus = focus', right = right' }
--   Just l, Nothing ->
--     let left' = l.tail
--         focus' = l.head : focus
--     in zr { focus = focus', left = left' }
--   Just l, Just r ->
--     let left' = l.tail
--         focus' = l.head : focus <> List.singleton r.head
--         right' = r.tail
--     in { left: left', focus: focus', right: right' }


-- focusArray :: forall a.
--               ZipperRange a
--            -> Array a
-- focusArray {focus} = Array.fromFoldable focus

-- unfocusArray :: forall a.
--                 Array a
--              -> ZipperRange a
-- unfocusArray arr = { left: Nil
--                    , focus: List.fromFoldable arr
--                    , right: Nil
--                    }


-- data UpdateView =
--     GoLeft
--   | GoRight
--   | Out
--   | In
--   | NoOp


-- btnScroll :: Event UpdateView
-- btnScroll = const GoLeft  <$> buttonEvent "scrollLeft" <|>
--             const GoRight <$> buttonEvent "scrollRight"

-- btnZoom :: Event UpdateView
-- btnZoom = const Out <$> buttonEvent "zoomOut" <|>
--           const In  <$> buttonEvent "zoomIn"

-- btnUpdateView :: Event UpdateView
-- btnUpdateView = btnScroll <|> btnZoom <|> (const NoOp <$> buttonEvent "redraw")



-- mkFoldView :: UpdateView
--            -> ZipperRange ChrId
--            -> ZipperRange ChrId
-- mkFoldView uv zr = case uv of
--   GoLeft  -> zipSlideLeft zr
--   GoRight -> zipSlideRight zr
--   Out     -> zipZoomOut zr
--   In      -> zipZoomIn zr
--   NoOp    -> zr


-- chrZREvent :: Array ChrId
--            -> Event UpdateView
--            -> Event (ZipperRange ChrId)
-- chrZREvent chrs uv = Event.fold mkFoldView uv (unfocusArray chrs)

-- chrsArrayEvent :: Event (ZipperRange ChrId)
--                -> Event (Array ChrId)
-- chrsArrayEvent = map (Array.fromFoldable <<< _.focus)

type BrowserView = Interval BrowserPoint



drawingEvent :: { min :: Number, max :: Number }
             -> CoordSys ChrId BrowserPoint
             -> { width :: Pixels, height :: Pixels, padding :: Pixels, yOffset :: Pixels }
             -> { gwas  :: Map ChrId (List _)
                , annots :: Map ChrId (List _) }
             -- -> Event (Array ChrId)
             -> Event (BrowserView)
             -> Event (Drawing)
drawingEvent s box dat = let dd = drawDemo' s 0.25 box dat
                         in map dd



chrsToView :: CoordSys ChrId BrowserPoint
           -> Array ChrId
           -> Interval BrowserPoint
chrsToView (CoordSys csys) view = fromMaybe (Interval (BPoint zero) (BPoint one)) $ do
  f <- Array.head view
  l <- Array.last view
  (Tuple _ ivS) <- Array.find ((==) f <<< fst) csys.intervals
  (Tuple _ ivE) <- Array.find ((==) l <<< fst) csys.intervals

  let (Interval start _) = ivS.interval
      (Interval _   end) = ivE.interval

  pure $ Interval start end



viewRange :: CoordSys ChrId BrowserPoint
          -> Event (Array ChrId)
          -> Event (Interval BrowserPoint)
viewRange chrs = map (chrsToView chrs)


clickEvent :: forall r. CanvasElement -> Event Pixels
clickEvent el = (_.x) <$> canvasEvent "mousedown" el


viewClick :: forall r. { width :: Pixels | r} -> Event Pixels -> Event (Ratio BigInt)
viewClick w = map (canvasToView w)


globalClick :: Event (Interval BrowserPoint) -> Event (Ratio BigInt) -> Event BrowserPoint
globalClick vs vx = (\iv r -> intervalToGlobal $ Local iv r) <$> vs <*> vx


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
            -> Event (Interval BrowserPoint)
            -> Event Number
            -> Event BrowserPoint
browserDrag w v ev = canvasToBrowserOffset w
                     <$> map intervalSize v
                     <*> ev



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


  let dragCanvasEv = canvasDrag canvas


  let viewEvent :: Event (Array ChrId)
      viewEvent = chrsArrayEvent $ chrZREvent mouseChrIds btnUpdateView

      viewEv' :: Event (Interval BrowserPoint)
      viewEv' = viewRange coordSys viewEvent

      click :: Event Pixels
      click = clickEvent canvas

      vClick :: Event (Ratio BigInt)
      vClick = viewClick {width: w} click

      gClick :: Event BrowserPoint
      gClick = globalClick viewEv' vClick

      fClick :: Event (Maybe _)
      fClick = frameClick coordSys gClick

      cClick :: Event (Maybe _)
      cClick = chrClick coordSys fClick

      clickEvs :: Event { view :: _
                        , canvasClick :: _
                        , vClick :: _
                        , gClick :: _
                        , fClick :: _
                        , cClick :: _ }
      clickEvs = { view: _, canvasClick: _, vClick: _
                 , gClick: _, fClick: _, cClick: _ }
                 <$> viewEv' <*> click <*> vClick
                 <*> gClick <*> fClick <*> cClick



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

  let sizes = {width: w, height: h, padding: 10.0, yOffset: 5.0}
      score = {min: 0.125, max: 0.42}

  let ev' = drawingEvent score sizes dat viewEvent
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
