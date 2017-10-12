module Genetics.Browser.UI.Native where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Random (random, randomRange)
import Control.Monad.Eff.Ref (Ref, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Rec.Class (forever)
import DOM (DOM)
import DOM.HTML.HTMLElement (getBoundingClientRect)
import DOM.HTML.Types (HTMLButtonElement, HTMLCanvasElement, HTMLElement)
import Data.Array (range, zip, zipWith, (..))
import Data.Int (round, toNumber)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (Tuple(..))
import FRP.Behavior (Behavior, behavior, step)
import FRP.Behavior as FRP
import FRP.Event (Event, subscribe)
import Genetics.Browser.Feature (Feature(..))
import Genetics.Browser.Glyph (Glyph, line, path, stroke)
import Genetics.Browser.GlyphF.Canvas (renderGlyph)
import Genetics.Browser.Units (Bp(..), Chr(..))
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)


type Point = { x :: Number
             , y :: Number }

type Fetch eff = Eff eff (Array (Feature Bp Number))


genF :: String -> Bp -> Bp -> Eff _ (Feature Bp Number)
genF chr' min max = do
  let chr = Chr chr'
  score <- randomRange 5.0 10.0
  pure $ Feature chr min max score

toF :: Int -> Number
toF = toNumber

randomFetch :: Int -> Chr -> Number -> Number -> Fetch _
randomFetch n chr min max = do
  let d = (max - min) / toNumber n
      bins = toNumber <$> 0 .. n
      rs = map (\x -> Tuple (x*d) (x*d+d)) bins

  traverse (\ (Tuple a b) -> genF "chr11" (Bp a) (Bp b)) rs


glyphify :: Feature Bp Number
         -> Glyph Unit
glyphify (Feature chr (Bp min) (Bp max) score) = do
  let height = 30.0
      color  = "red"
      midX   = min + ((max - min) / 2.0)
  stroke color
  path [{x: min, y: score}, {x:midX, y: score+height}, {x: max, y: score}]


visible :: View -> Bp -> Boolean
visible v x = v.min > x && x < v.max

hToScreen :: View -> Bp -> Number
hToScreen v x = unwrap $ v.min + (offset / (v.max - v.min))
  where offset = x - v.min

glyphifyWithView :: View
                 -> Feature Bp Number
                 -> Glyph Unit
glyphifyWithView v (Feature chr min' max' score) = do
  when (not visible v min' && not visible v max') $ pure unit

  let height = 30.0
      color  = "red"
      min    = hToScreen v min'
      max    = hToScreen v max'
      midX   = min + ((max - min) / 2.0)
  stroke color
  path [{x: min, y: score}, {x:midX, y: score+height}, {x: max, y: score}]


canvasElementToHTML :: CanvasElement -> HTMLElement
canvasElementToHTML = unsafeCoerce

type View = { min :: Bp
            , max :: Bp
            , cWidth :: Number
            , cHeight :: Number
            } 

fetchWithView :: Int -> View -> Fetch _
fetchWithView n v = randomFetch n (Chr "chr11") (unwrap v.min) (unwrap v.max)


fetchToCanvas :: (View -> Fetch _) -> View -> Context2D -> Eff _ Unit
fetchToCanvas f v ctx = do
  features <- f v 
  let gs = traverse_ glyphify features
  renderGlyph ctx gs


foreign import animationFrameLoop :: forall eff.
                                     Eff eff Unit
                                  -> Eff eff Unit

foreign import clearCanvas :: forall eff.
                              Number
                           -> Number
                           -> Context2D
                           -> Eff eff Unit

foreign import setButtonEvent :: forall eff.
                                 String
                              -> Eff eff Unit
                              -> Eff eff Unit

foreign import getScreenSize :: forall eff. Eff eff { w :: Number, h :: Number }


animate :: Number
        -> Number
        -> (View -> Fetch _)
        -> CanvasElement
        -> Ref { prev :: View, cur :: View }
        -> Eff _ Unit
animate w h f canvas vRef = do
  ctx <- getContext2D canvas
  animationFrameLoop do
    v <- readRef vRef
    when (v.cur.min /= v.prev.min ||
          v.cur.max /= v.prev.max) do
        clearCanvas w h ctx
        modifyRef vRef (_ { prev = v.cur })
        fetchToCanvas f v.cur ctx


scrollView :: Bp -> View -> View
scrollView bp v = v { min = v.min + bp, max = v.max + bp }


-- 1st element is a backbuffer, 2nd the one shown on screen
foreign import scrollCanvas :: forall eff.
                               CanvasElement
                            -> CanvasElement
                            -> Number
                            -> Eff eff Unit

-- creates a new CanvasElement, not attached to the DOM and thus not visible
foreign import newCanvas :: forall eff.
                            { w :: Number, h :: Number }
                         -> Eff eff CanvasElement


foreign import canvasDrag :: CanvasElement -> Event Point

foreign import canvasEvents :: CanvasElement
                            -> { click :: Event Point
                               , mouseup :: Event Point
                               , mousedown :: Event Point
                               , drag  :: Event Point
                               }




scrollDeriv :: CanvasElement -> Behavior Point
scrollDeriv c = step {x: 0.0, y: 0.0} (canvasDrag c)

xB :: Behavior Point -> Behavior Number
xB = map (\{x,y} -> x)

scaleViewBehavior :: Number -> Behavior Bp -> Behavior Bp
scaleViewBehavior s b = (wrap <<< (_ * s) <<< unwrap) <$> b
main :: Eff _ Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas

  {w,h} <- getScreenSize
  _ <- setCanvasWidth (w-2.0) canvas

  log $ "canvas width: " <> show w

  offset <- getBoundingClientRect $ canvasElementToHTML canvas

  backCanvas <- newCanvas {w,h}

  let minView = Bp 0.0
      maxView = Bp w
      v :: View
      v = { min: minView, max: maxView, scale: BpPerPixel 1.0 }
      f :: View -> Fetch _
      f = fetchWithView 100

  vRef <- newRef { cur: v, prev: v }

  let events = canvasEvents canvas

  _ <- subscribe events.drag \{x,y} -> do
    scrollCanvas backCanvas canvas (-x)
    log $ "x: " <> show x <> "\t\ty: " <> show y

  _ <- subscribe events.mouseup \{x,y} -> do
    v' <- readRef vRef
    writeRef vRef (v' { prev = v'.cur })
    fetchToCanvas f v'.cur ctx



  setButtonEvent "scrollLeft" do
    scrollCanvas backCanvas canvas (-100.0)
    -- v <- readRef vRef
    -- let newView = scrollView (Bp (-100.0)) v.cur
    -- log $ "scrolling left, to " <> show newView.min
    -- writeRef vRef { cur: newView, prev: v.cur }

  setButtonEvent "scrollRight" do
    scrollCanvas backCanvas canvas 100.0
    -- v <- readRef vRef
    -- let newView = scrollView (Bp 100.0) v.cur
    -- log $ "scrolling right, to " <> show newView.min
    -- writeRef vRef { cur: newView, prev: v.cur }

  -- render first frame
  fetchToCanvas f v ctx

  animate w h f canvas vRef
