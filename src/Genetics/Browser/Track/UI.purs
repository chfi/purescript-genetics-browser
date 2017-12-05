module Genetics.Browser.Track.UI where

import Prelude

import Control.Alt ((<|>))
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
import Data.Either (Either(..))
import Data.Filterable (class Filterable, filter, filterMap)
import Data.Filterable as Filterable
import Data.Foldable (class Foldable, foldMap, for_, length, maximum, maximumBy, sum)
import Data.Int (round)
import Data.Lens ((^?))
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
import Data.Semigroup.Foldable (foldMap1)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Debug.Trace (traceShow)
import FRP.Event (Event)
import FRP.Event as Event
import FRP.Event as FRP
import Genetics.Browser.Track.Backend (GWASFeature, Gene, drawDemo, getDataDemo, mouseChrIds)
import Genetics.Browser.Types (Bp(..), ChrId(..))
import Genetics.Browser.View (Pixels)
import Global as Global
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, Context2D, TranslateTransform, Transform, getCanvasElementById, getCanvasHeight, getContext2D, setCanvasWidth, transform, translate, withContext)
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, white)
import Graphics.Drawing as Drawing
import Math as Math
import Network.HTTP.Affjax as Affjax
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

foreign import getScreenSize :: forall eff. Eff eff { w :: Number, h :: Number }

-- 1st element is a backbuffer, 2nd the one shown on screen
-- foreign import scrollCanvas :: forall eff.
--                                CanvasElement
--                             -> CanvasElement
--                             -> Point
--                             -> Eff eff Unit

-- creates a new CanvasElement, not attached to the DOM and thus not visible
-- foreign import newCanvas :: forall eff.
--                             { w :: Number, h :: Number }
--                          -> Eff eff CanvasElement

foreign import clearCanvas :: forall eff. CanvasElement -> Eff eff Unit


-- set an event to fire on the given button id
foreign import buttonEvent :: String
                           -> Event Unit

-- foreign import canvasDragImpl :: CanvasElement -> Event { during :: Nullable Point
--                                                         , total :: Nullable Point }

-- foreign import canvasEvent :: String -> CanvasElement -> Event Point


foreign import setViewUI :: forall eff. String -> Eff eff Unit


type View = { lHand :: ChrId, rHand :: ChrId }

type ZipperRange a =
  { left  :: List a
  , focus :: List a
  , right :: List a
  }

zipSlideLeft :: forall a.
                ZipperRange a
             -> ZipperRange a
zipSlideLeft zr@{left, focus, right} = case List.uncons left, List.unsnoc focus of
    Nothing, _  -> zr
    _ , Nothing -> zr
    Just l, Just f ->
      let left'  = l.tail
          focus' = l.head : f.init
          right' = f.last : right
      in {left: left', focus: focus', right: right'}

zipSlideRight :: forall a.
                 ZipperRange a
              -> ZipperRange a
zipSlideRight zr@{left, focus, right} = case List.uncons focus, List.uncons right of
    Nothing, _  -> zr
    _ , Nothing -> zr
    Just f, Just r ->
      let left'  = f.head : left
          focus' = f.tail <> List.singleton r.head
          right' = r.tail
      in {left: left', focus: focus', right: right'}


zipZoomIn :: forall a.
             ZipperRange a
          -> ZipperRange a
zipZoomIn zr@{left, focus, right} = case List.uncons focus of
  Nothing -> zr
  Just {head, tail} -> case List.unsnoc tail of
    Nothing -> zr
    Just {init, last} ->
      let left'  = head : left
          focus' = init
          right' = last : right
      in {left: left', focus: focus', right: right'}


zipZoomOut :: forall a.
              ZipperRange a
           -> ZipperRange a
zipZoomOut zr@{left, focus, right} = case List.uncons left, List.uncons right of
  Nothing, Nothing -> zr
  Nothing, Just r ->
    let focus' = focus <> List.singleton r.head
        right' = r.tail
    in zr { focus = focus', right = right' }
  Just l, Nothing ->
    let left' = l.tail
        focus' = l.head : focus
    in zr { focus = focus', left = left' }
  Just l, Just r ->
    let left' = l.tail
        focus' = l.head : focus <> List.singleton r.head
        right' = r.tail
    in { left: left', focus: focus', right: right' }


focusArray :: forall a.
              ZipperRange a
           -> Array a
focusArray {focus} = Array.fromFoldable focus

unfocusArray :: forall a.
                Array a
             -> ZipperRange a
unfocusArray arr = { left: Nil
                   , focus: List.fromFoldable arr
                   , right: Nil
                   }


data UpdateView =
    GoLeft
  | GoRight
  | Out
  | In
  | NoOp


btnScroll :: Event UpdateView
btnScroll = const GoLeft  <$> buttonEvent "scrollLeft" <|>
            const GoRight <$> buttonEvent "scrollRight"

btnZoom :: Event UpdateView
btnZoom = const Out <$> buttonEvent "zoomOut" <|>
          const In  <$> buttonEvent "zoomIn"

btnUpdateView :: Event UpdateView
btnUpdateView = btnScroll <|> btnZoom <|> (const NoOp <$> buttonEvent "redraw")



mkFoldView :: UpdateView
           -> ZipperRange ChrId
           -> ZipperRange ChrId
mkFoldView uv zr = case uv of
  GoLeft  -> zipSlideLeft zr
  GoRight -> zipSlideRight zr
  Out     -> zipZoomOut zr
  In      -> zipZoomIn zr
  NoOp    -> zr


chrZREvent :: Array ChrId
           -> Event UpdateView
           -> Event (ZipperRange ChrId)
chrZREvent chrs uv = Event.fold mkFoldView uv (unfocusArray chrs)

chrsArrayEvent :: Event (ZipperRange ChrId)
               -> Event (Array ChrId)
chrsArrayEvent = map (Array.fromFoldable <<< _.focus)


drawingEvent :: { min :: Number, max :: Number }
             -> { width :: Pixels, height :: Pixels, padding :: Pixels, yOffset :: Pixels }
             -> { gwas  :: Map ChrId (List _)
                , annots :: Map ChrId (List _) }
             -> Event (Array ChrId)
             -> Event (Drawing)
drawingEvent s box dat = let dd = drawDemo s 0.25 box dat
                         in map dd


main :: Eff _ _
main = launchAff do

  canvas <- liftEff $ unsafePartial $ fromJust <$> getCanvasElementById "canvas"
  ctx <- liftEff $ getContext2D canvas

  {w,h} <- liftEff do
    {w} <- getScreenSize
    h <- getCanvasHeight canvas
    _ <- setCanvasWidth (w-2.0) canvas
    pure {w, h}

  let height = h
      offset = 0.0
      yInfo = { height, offset }

      chrIds = mouseChrIds


  let viewEvent = chrsArrayEvent $ chrZREvent mouseChrIds btnUpdateView

  void $ liftEff $ Event.subscribe viewEvent
       (\arr -> do
           let l = fromMaybe "N/A" $ show <$> Array.head arr
               r = fromMaybe "N/A" $ show <$> Array.last arr
           setViewUI $ l <> "\tto\t" <> r)

  dat <- getDataDemo { gwas: "./gwas.json"
                     , annots: "./annots_fake.json" }

  let sizes = {width: w, height: h, padding: 10.0, yOffset: 5.0}
      score = {min: 0.125, max: 0.42}

  let ev' = drawingEvent score sizes dat viewEvent
      bg = filled (fillColor white) $ rectangle 0.0 0.0 w h

  void $ liftEff $ Event.subscribe ev' (\d -> Drawing.render ctx (bg <> d))
