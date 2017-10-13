module Genetics.Browser.UI.Native where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Random (randomRange)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Error.Class (throwError)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap)
import Data.Int (toNumber)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe, Maybe(Just, Nothing), fromJust, isNothing)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import FRP.Event (Event, sampleOn_)
import FRP.Event as FRP
import Genetics.Browser.Feature (Feature(..))
import Genetics.Browser.Glyph (Glyph, circle, path, stroke)
import Genetics.Browser.GlyphF.Canvas (renderGlyph)
import Genetics.Browser.GlyphF.Log (showGlyph)
import Genetics.Browser.Units (Bp(..), BpPerPixel(..), Chr(..), bpToPixels, pixelsToBp)
import Global as Global
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D, setCanvasWidth)
import Math as Math
import Network.HTTP.Affjax as Affjax
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


fileFetch :: String
          -> View
          -- -> Aff _ Json
          -> Fetch _
fileFetch url view = do

  json <- _.response <$> Affjax.get url

  let f :: Json -> Maybe (Feature Bp Number)
      f j = do
            obj <- j ^? _Object
            min <- Bp <$> obj ^? ix "min" <<< _Number
            max <- Bp <$> obj ^? ix "max" <<< _Number
        -- TODO this might get fucked up due to exponential notation
            score <- Global.readFloat <$> obj ^? ix "pValue" <<< _String
            pure $ Feature (Chr "chr11") min max ((-5.0) * (Math.log score / Math.log 10.0))

  -- pure json
  case traverse f =<< json ^? _Array of
    -- TODO throw error on Nothing
    Nothing -> throwError $ error "Failed to parse JSON features"
    Just fs -> pure $ fs


glyphify :: View
         -> Feature Bp Number
         -> Glyph Unit
glyphify v (Feature chr min max score) = do
  let height = 30.0
      color  = "red"
      x = bpToPixels v.scale (min - v.min)
      -- x = unwrap min
      -- midX   = min + ((max - min) / 2.0)
  stroke color
  circle { x: x, y: height } 3.0
  -- path [{x: min, y: score}, {x:midX, y: score+height}, {x: max, y: score}]
  -- path [{x: min, y: score}, {x:midX, y: score+height}, {x: max, y: score}]


visible :: View -> Bp -> Boolean
visible v x = v.min > x && x < v.max

hToScreen :: View -> Bp -> Number
hToScreen v x = unwrap $ v.min + (offset / (v.max - v.min))
  where offset = x - v.min


canvasElementToHTML :: CanvasElement -> HTMLElement
canvasElementToHTML = unsafeCoerce


fetchWithView :: Int -> View -> Fetch _
fetchWithView n v = randomFetch n (Chr "chr11") (unwrap v.min) (unwrap v.max)


fetchToCanvas :: (View -> Fetch _) -> View -> Context2D -> Aff _ Unit
fetchToCanvas f v ctx = do
  features <- f v
  let gs = traverse_ (glyphify v) features
  -- liftEff $ log $ showGlyph gs
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

foreign import canvasDragImpl :: CanvasElement -> Event { during :: Nullable Point
                                                        , total :: Nullable Point }
-- foreign import canvasDragImpl :: CanvasElement -> Event Point

foreign import canvasEvent :: String -> CanvasElement -> Event Point

canvasDrag :: CanvasElement -> Event (Either Point Point)
canvasDrag el = f <$> canvasDragImpl el
  where f ev = case toMaybe ev.during of
          Just p  -> Right p
          Nothing -> Left $ unsafePartial $ fromJust (toMaybe ev.total)

  -- toMaybe <$> canvasDragImpl el


-- Given an event of dragging a canvas ending with Nothing,
-- produces an event of the total horizontal dragged distance when mouse is let go
-- horDragEv :: Event (Maybe Point) -> Event Number
-- horDragEv ev = ptSum `sampleOn_` doneEv
--   where doneEv = filter isNothing ev
--         ptSum = FRP.fold (+) (filterMap (map _.x) ev) 0.0


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

        f (ScrollPixels x) v' = v' { min = v'.min - (pixelsToBp v'.scale x)
                                   , max = v'.max - (pixelsToBp v'.scale x) }

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

  let minView = Bp 200000.0
      maxView = Bp 300000.0
      s :: BpPerPixel
      s = BpPerPixel $ 100000.0 / w
      v :: View
      v = { min: minView, max: maxView, scale: s }
      f :: View -> Fetch _
      f = fileFetch "http://localhost:8080/gwas.json"
      -- f = fetchWithView 100


  let cDrag = canvasDrag canvas
      lefts = case _ of
        Left x -> Just x
        Right _ -> Nothing
      -- the alt operator <|> combines the two event streams,
      -- resulting in an event of both button-click scrolls
      -- and canvas-drag scrolls
      scroll = filterMap (map (ScrollPixels <<< _.x) <<< lefts) cDrag
      updateViews = btnScroll (Bp 500.0) <|>
                    scroll
                    -- (map ScrollPixels) cDrag
      viewB = viewBehavior updateViews v


  _ <- liftEff $ unsafeCoerceEff $ FRP.subscribe cDrag $ case _ of
    Left _      -> pure unit
    Right {x,y} -> scrollCanvas backCanvas canvas {x: -x, y: 0.0}

  _ <- liftEff $ unsafeCoerceEff $ FRP.subscribe viewB \v' -> do
    clearCanvas canvas
    launchAff $ fetchToCanvas f v' ctx

  -- render first frame
  fetchToCanvas f v ctx


main :: Eff _ _
main = launchAff do
  x <- fileFetch "http://localhost:8080/gwas.json" (unsafeCoerce unit)
  liftEff $ log $ unsafeStringify x
  browser
