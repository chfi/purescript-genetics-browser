module Genetics.Browser.UI.Native where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (randomRange)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import DOM.HTML.Types (HTMLElement)
import Data.Array ((..))
import Data.Filterable (filter, filterMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing), fromJust, isNothing)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import FRP.Event (Event, sampleOn_)
import FRP.Event as FRP
import Genetics.Browser.Feature (Feature(..))
import Genetics.Browser.Glyph (Glyph, path, stroke)
import Genetics.Browser.GlyphF.Canvas (renderGlyph)
import Genetics.Browser.Units (Bp(..), BpPerPixel(..), Chr(..), pixelsToBp)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D, setCanvasWidth)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)


type Point = { x :: Number
             , y :: Number }

type Fetch eff = Aff eff (Array (Feature Bp Number))


genF :: String -> Bp -> Bp -> Aff _ (Feature Bp Number)
genF chr' min max = liftEff do
  let chr = Chr chr'
  score <- randomRange 5.0 10.0
  pure $ Feature chr min max score

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


fetchWithView :: Int -> View -> Fetch _
fetchWithView n v = randomFetch n (Chr "chr11") (unwrap v.min) (unwrap v.max)


fetchToCanvas :: (View -> Fetch _) -> View -> Context2D -> Aff _ Unit
fetchToCanvas f v ctx = do
  features <- f v
  let gs = traverse_ glyphify features
  liftEff $ renderGlyph ctx gs


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

foreign import canvasDragImpl :: CanvasElement -> Event (Nullable Point)

foreign import canvasEvent :: String -> CanvasElement -> Event Point

canvasDrag :: CanvasElement -> Event (Maybe Point)
canvasDrag el = toMaybe <$> canvasDragImpl el


-- Given an event of dragging a canvas ending with Nothing,
-- produces an event of the total horizontal dragged distance when mouse is let go
horDragEv :: Event (Maybe Point) -> Event Number
horDragEv ev = ptSum `sampleOn_` doneEv
  where doneEv = filter isNothing ev
        ptSum = FRP.fold (+) (filterMap (map _.x) ev) 0.0


type View = { min :: Bp
            , max :: Bp
            , scale :: BpPerPixel
            }

data UpdateView = ScrollBp Bp
                | ScrollPixels Number
                | SetRange Bp Bp
                | ModScale (BpPerPixel -> BpPerPixel)
                | SetScale BpPerPixel

viewBehavior :: Event UpdateView
             -> View
             -> Event View
viewBehavior ev v = FRP.fold f ev v
  where f :: UpdateView -> View -> View
        f (ScrollBp x) v' = v' { min = v'.min + x
                               , max = v'.max + x }

        f (ScrollPixels x) v' = v' { min = v'.min + (pixelsToBp v'.scale x)
                                   , max = v'.max + (pixelsToBp v'.scale x) }

        f (SetRange min' max') v' = v' { min = min'
                                       , max = max' }

        f (ModScale g) v' = v' { scale = g v'.scale }

        f (SetScale s) v' = v' { scale = s }


-- how far to scroll when clicking a button
btnScroll :: Bp -> Event UpdateView
btnScroll x = f' (-x) <$> buttonEvent "scrollLeft" <|>
              f'   x  <$> buttonEvent "scrollRight"
  where f' = const <<< ScrollBp


-- TODO: set a range to jump the view to
-- TODO: zoom in and out
-- TODO: set zoom/scale
-- TODO: set just one side of the view?


browser :: Aff _ Unit
browser = do
  canvas <- liftEff $ unsafePartial $ fromJust <$> getCanvasElementById "canvas"
  ctx <- liftEff $ getContext2D canvas

  {w,h} <- liftEff $ getScreenSize
  _ <- liftEff $ setCanvasWidth (w-2.0) canvas
  backCanvas <- liftEff $ newCanvas {w,h}

  let minView = Bp 0.0
      maxView = Bp w
      v :: View
      v = { min: minView, max: maxView, scale: BpPerPixel 1.0 }
      f :: View -> Fetch _
      f = fetchWithView 100

  let cDrag = canvasDrag canvas
      -- the alt operator <|> combines the two event streams,
      -- resulting in an event of both button-click scrolls
      -- and canvas-drag scrolls
      updateViews = btnScroll (Bp 500.0) <|>
                    (map ScrollPixels <<< horDragEv) cDrag
      viewB = viewBehavior updateViews v

  _ <- liftEff $ unsafeCoerceEff $ FRP.subscribe cDrag $ case _ of
    Nothing -> pure unit
    Just {x,y}  -> scrollCanvas backCanvas canvas {x: -x, y: 0.0}

  _ <- liftEff $ unsafeCoerceEff $ FRP.subscribe viewB \v' -> do
    clearCanvas canvas
    launchAff $ fetchToCanvas f v' ctx

  -- render first frame
  fetchToCanvas f v ctx


main = launchAff browser
