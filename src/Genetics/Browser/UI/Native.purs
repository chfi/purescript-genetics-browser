module Genetics.Browser.UI.Native where

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
import Data.Foldable (class Foldable, for_, length)
import Data.Int (round)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (over, unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import Debug.Trace (traceShow)
import FRP.Event (Event)
import FRP.Event as FRP
import Genetics.Browser.Feature (Feature(..))
import Genetics.Browser.Glyph (Glyph, circle, fill, stroke)
import Genetics.Browser.GlyphF.Canvas (renderGlyph)
import Genetics.Browser.GlyphF.Log (showGlyph)
import Genetics.Browser.Types (Point)
import Genetics.Browser.UI.Native.GlyphBounds (clickAnnGlyphs)
import Genetics.Browser.UI.Native.View (UpdateView(..), View, browserTransform, foldView)
import Genetics.Browser.UI.Native.View as View
import Genetics.Browser.Units (Bp(Bp), BpPerPixel(BpPerPixel), Chr(Chr), bpToPixels)
import Global as Global
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, Context2D, TranslateTransform, getCanvasElementById, getCanvasHeight, getContext2D, setCanvasWidth, translate, withContext)
import Math as Math
import Network.HTTP.Affjax as Affjax
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)


type Fetch eff = Aff eff (Array (Feature Bp Number))


fileFetch :: String
          -> View
          -> Fetch _
fileFetch url _ = do
  json <- _.response <$> Affjax.get url

  let f :: Json -> Maybe (Feature Bp Number)
      f j = do
            obj <- j ^? _Object
            min <- Bp <$> obj ^? ix "min" <<< _Number
            max <- Bp <$> obj ^? ix "max" <<< _Number
            -- score <- Global.readFloat <$> obj ^? ix "pValue" <<< _String
            score <- obj ^? ix "score" <<< _Number
            -- pure $ Feature (Chr "chr11") min max ((-2.0) * (Math.log score / Math.log 10.0))
            -- pure $ Feature (Chr "chr11") min max ((-2.0) * (Math.log score / Math.log 10.0))
            pure $ Feature (Chr "chr11") min max (score * 100.0)

  case traverse f =<< json ^? _Array of
    Nothing -> throwError $ error "Failed to parse JSON features"
    Just fs -> pure $ fs


glyphifyFeatures :: Array (Feature Bp Number)
                 -> View
                 -> Array (Glyph Unit)
glyphifyFeatures fs v' = map (g v') fs
  where g v (Feature chr min max score) = do
            let y = score
                x = bpToPixels v.scale (min - v.min)
                -- z = traceShow x \_ -> unit
                -- x = 100.0
                -- x = 100.0
            stroke "black"
            fill "red"
            circle { x, y } 3.0

-- add function (Array (Glyph Unit)) -> Context2D -> Context2D -> Eff _ Unit,
-- where a single glyph is drawn many times by filling a backbuffer and copying it.
-- probably won't work with GlyphF; the glyphs are in fact not the same,
-- each one has its own position etc.
-- can we have a way to simplify this so each track only gets one glyph?
-- an explicit mapping from feature to pair of glyph and offset, or something like that.
-- then we could construct a tree, with the stroke & fill at the root. woop.
-- worth thinking about. & reading about optimizing these things
-- basically traversing/sequence something like
-- [Stroke "black" (Circle _ _ _), Stroke "black" (Circle _ _ _) ...]
-- into
-- Stroke "black" [Circle _ _ _, Circle _ _ _...]
-- though that is fundamentally different from the idea of drawing a single glyph
-- to a backbuffer.
-- *that* idea makes more sense with the idea of a glyph being somehow atomic;
-- a glyph should only describe what it looks like, in its own system -- not its
-- height, nothing like that. hmm.
-- the glyph would be more a function of the track than the feature.
-- however, it shouldn't necessarily be so; a track should be able to display different
-- glyphs depending on the feature values.
-- Buuut they should also memoize based on feature, so if the features in a track
-- only produce like 10 glyphs that are identical except for color, then those
-- renderings should be optimized.
-- need research!!!

filterFeatures :: forall f a b.
                  Filterable f
               => View
               -> f (Tuple (Feature Bp b) a)
               -> f a
filterFeatures view = map snd <<< Filterable.filter f
  where f :: _
        f (Tuple (Feature _ min max _) g) = min > view.min && max < view.max


renderGlyphs :: forall f a.
                Foldable f
             => TranslateTransform
             -> Context2D
             -> View
             -> f (Glyph a)
             -> Eff _ Unit
renderGlyphs tt ctx v gs = withContext ctx do
  void $ translate tt ctx
  foreachE (Array.fromFoldable gs) $ void <$> renderGlyph ctx


canvasElementToHTML :: CanvasElement -> HTMLElement
canvasElementToHTML = unsafeCoerce

foreign import getScreenSize :: forall eff. Eff eff { w :: Number, h :: Number }

-- 1st element is a backbuffer, 2nd the one shown on screen
foreign import scrollCanvas :: forall eff.
                               CanvasElement
                            -> CanvasElement
                            -> Point
                            -> Eff eff Unit

-- creates a new CanvasElement, not attached to the DOM and thus not visible
foreign import newCanvas :: forall eff.
                            { w :: Number, h :: Number }
                         -> Eff eff CanvasElement

foreign import clearCanvas :: forall eff. CanvasElement -> Eff eff Unit


-- set an event to fire on the given button id
foreign import buttonEvent :: String
                           -> Event Unit

foreign import canvasDragImpl :: CanvasElement -> Event { during :: Nullable Point
                                                        , total :: Nullable Point }

foreign import canvasEvent :: String -> CanvasElement -> Event Point

canvasDrag :: CanvasElement -> Event (Either Point Point)
canvasDrag el = f <$> canvasDragImpl el
  where f ev = case toMaybe ev.during of
          Just p  -> Right p
          Nothing -> Left $ unsafePartial $ fromJust (toMaybe ev.total)


-- how far to scroll when clicking a button
btnScroll :: Bp -> Event UpdateView
btnScroll x = f' (-x) <$> buttonEvent "scrollLeft" <|>
              f'   x  <$> buttonEvent "scrollRight"
  where f' = const <<< ScrollBp

btnZoom :: (BpPerPixel -> BpPerPixel)
        -> (BpPerPixel -> BpPerPixel)
        -> Event UpdateView
btnZoom zIn zOut = f' zOut <$> buttonEvent "zoomOut" <|>
                   f' zIn  <$> buttonEvent "zoomIn"
  where f' = const <<< ModScale

foreign import setViewUI :: forall eff. String -> Eff eff Unit

-- TODO: set a range to jump the view to
-- DONE: zoom in and out
-- TODO: set zoom/scale
-- TODO: window/canvas resizing

foreign import canvasWheelEvent :: CanvasElement -> Event Number

scrollZoom :: CanvasElement -> Event UpdateView
scrollZoom el = map (ModScale <<< f) $ canvasWheelEvent el
  where f :: Number -> (BpPerPixel -> BpPerPixel)
        f dY = over BpPerPixel $ (_ * (1.0 + (dY / 30.0)))


-- TODO: fetching could (should) be done completely async,
-- and done on subscribing to the Event View. Like now,
-- but less clumsy -- model rendering around the fetch function,
-- and add some caching to it

-- fetcho :: String -> Event View -> Aff _ _
-- fetcho uri ev = launchAff do


browser :: String -> Aff _ Unit
browser uri = do
  canvas <- liftEff $ unsafePartial $ fromJust <$> getCanvasElementById "canvas"
  ctx <- liftEff $ getContext2D canvas

  {w,h} <- liftEff do
    {w} <- getScreenSize
    h <- getCanvasHeight canvas
    _ <- setCanvasWidth (w-2.0) canvas
    pure {w, h}

  backCanvas <- liftEff $ newCanvas {w,h}


  browserState <- makeVar

  let minView = Bp 20000.0
      maxView = Bp 3000000.0
      v = View.fromCanvasWidth w { min: minView, max: maxView }
      f :: View -> Fetch _
      f = fileFetch uri
      renderer :: _
      -- renderer = renderGlyphs (browserTransform h) ctx
      renderer = renderGlyphs {translateX: 0.0, translateY: 0.0} ctx
      fetch :: View -> Aff _ _
      fetch v = do


        fs <- f v
        let gs = glyphifyFeatures fs v
            annGs = zip fs gs

        liftEff $ log $ unsafeStringify gs

        putVar browserState { annotatedGlyphs: annGs
                            , view: v }
        liftEff do
          clearCanvas canvas
          renderer v gs'


  let cDrag = canvasDrag canvas
      lefts = case _ of
        Left x -> Just x
        Right _ -> Nothing
      -- the alt operator <|> combines the two event streams,
      -- resulting in an event of both button-click scrolls
      -- and canvas-drag scrolls

      dragScroll = filterMap (map (ScrollPixels <<< _.x) <<< lefts) cDrag
      updateViews = btnScroll (Bp 500.0)
                <|> dragScroll
                <|> btnZoom (wrap <<< (_ * 0.8) <<< unwrap)
                            (wrap <<< (_ * 1.2) <<< unwrap)
                <|> scrollZoom canvas
      viewB = FRP.fold foldView updateViews v


  _ <- liftEff $ unsafeCoerceEff $ FRP.subscribe cDrag $ case _ of
    Left _      -> pure unit
    Right {x,y} -> scrollCanvas backCanvas canvas {x: -x, y: 0.0}


  _ <- liftEff $ unsafeCoerceEff $ FRP.subscribe viewB \v' -> do

    _ <- launchAff $ fetch v'

    setViewUI $ "View range: "
             <> show (round $ unwrap v'.min) <> " - "
             <> show (round $ unwrap v'.max)

  -- thread rendering view
  -- _ <- forkAff $ forever do
    -- render when the view & features have been updated
    -- state <- peekVar browserState
    -- liftEff do



  -- _ <- liftEff $ unsafeCoerceEff $
  --      FRP.subscribe (canvasEvent "click" canvas) \p -> launchAff do
  --        state <- tryPeekVar browserState
  --        liftEff $ log $ "clicked: (" <> show p.x <> ", " <> show p.y <> ")"
  --        case state of
  --          Nothing -> liftEff $ log "no state!"
  --          Just s' -> do
  --            liftEff $ log "state!"
  --            let gs' :: _
  --                gs' = clickAnnGlyphs s'.annotatedGlyphs p

  --            traverse_ (liftEff <<< log <<< showGlyph <<< snd) gs'


  -- render first frame
  fetch v




main :: Eff _ _
main = launchAff $ browser "./alot.json"
