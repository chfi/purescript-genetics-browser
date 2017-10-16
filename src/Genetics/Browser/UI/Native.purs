module Genetics.Browser.UI.Native where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, delay, forkAff, launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Random (randomRange)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Trans (StateT(..), evalStateT, execStateT, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (Writer, execWriter, tell)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array ((..))
import Data.Either (Either(..))
import Data.Filterable (class Filterable, filter, filterMap)
import Data.Int (round, toNumber)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe, Maybe(Just, Nothing), fromJust, isNothing)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence_, traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import FRP (FRP)
import FRP.Event (Event, sampleOn_)
import FRP.Event as FRP
import Genetics.Browser.Feature (Feature(..))
import Genetics.Browser.Glyph (Glyph, circle, fill, path, stroke)
import Genetics.Browser.GlyphF (GlyphF(..))
import Genetics.Browser.GlyphF.Canvas (renderGlyph)
import Genetics.Browser.GlyphF.Log (showGlyph)
import Genetics.Browser.Units (Bp(..), BpPerPixel(..), Chr(..), bpToPixels, pixelsToBp)
import Global as Global
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getCanvasHeight, getContext2D, setCanvasWidth)
import Math as Math
import Network.HTTP.Affjax as Affjax
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)


type Point = { x :: Number
             , y :: Number }

type Fetch eff = Aff eff (Array (Feature Bp Number))


fileFetch :: String
          -> View
          -> Fetch _
fileFetch url view = do
  json <- _.response <$> Affjax.get url

  let f :: Json -> Maybe (Feature Bp Number)
      f j = do
            obj <- j ^? _Object
            min <- Bp <$> obj ^? ix "min" <<< _Number
            max <- Bp <$> obj ^? ix "max" <<< _Number
            score <- Global.readFloat <$> obj ^? ix "pValue" <<< _String
            pure $ Feature (Chr "chr11") min max ((-2.0) * (Math.log score / Math.log 10.0))

  case traverse f =<< json ^? _Array of
    Nothing -> throwError $ error "Failed to parse JSON features"
    Just fs -> pure $ fs


glyphify :: Number
         -> View
         -> Feature Bp Number
         -> Glyph Unit
glyphify h v (Feature chr min max score) = do
  let height = 30.0
      y = h - (score + height)
      x = bpToPixels v.scale (min - v.min)
  stroke "black"
  fill "red"
  circle { x, y } 3.0



canvasElementToHTML :: CanvasElement -> HTMLElement
canvasElementToHTML = unsafeCoerce

fetchToCanvas :: Number -> (View -> Fetch _) -> View -> Context2D -> Aff _ Unit
fetchToCanvas h f v ctx = do
  features <- f v
  let gs = traverse_ (glyphify h v) features
  liftEff $ renderGlyph ctx gs


type TrackState = Array (Tuple (Feature Bp Number) (Glyph Unit))

fetchToCanvas' :: Number
               -> (View -> Fetch _)
               -> View
               -> Context2D
               -> StateT TrackState (Aff _) Unit
fetchToCanvas' h f v ctx = do
  features <- liftAff $ f v
  let gs :: TrackState
      gs = map (\f -> Tuple f (glyphify h v f)) features
  put gs
  liftEff $ renderGlyph ctx $ traverse_ snd gs
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



type View = { min :: Bp
            , max :: Bp
            , scale :: BpPerPixel
            }

data UpdateView = ScrollBp Bp
                | ScrollPixels Number
                | SetRange Bp Bp
                | ModScale (BpPerPixel -> BpPerPixel)
                | SetScale BpPerPixel

modScale :: View
         -> (BpPerPixel -> BpPerPixel)
         -> View
modScale v f = { scale, min: mid - d, max: mid + d }
  where bpsWide = v.max - v.min
        pixelsWide = bpToPixels v.scale bpsWide
        mid = v.min + ((v.max - v.min) * Bp 0.5)
        scale = f v.scale
        bpsWide' = pixelsToBp scale pixelsWide
        d = bpsWide' * Bp 0.5

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

        f (ModScale g) v' = modScale v' g

        f (SetScale s) v' = modScale v' (const s)


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


widthToView :: Number
            -> { min :: Bp, max :: Bp }
            -> View
widthToView w { min, max } = { min, max, scale }
  where scale = BpPerPixel $ (unwrap (max - min)) / w

foreign import canvasWheelEvent :: CanvasElement -> Event Number

scrollZoom :: CanvasElement -> Event UpdateView
scrollZoom el = map (ModScale <<< f) $ canvasWheelEvent el
  where f :: Number -> (BpPerPixel -> BpPerPixel)
        f dY = over BpPerPixel $ (_ * (1.0 + (dY / 30.0)))


newtype GlyphBounds = GlyphBounds (Point -> Boolean)

derive instance newtypeGlyphBounds :: Newtype GlyphBounds _

instance semigroupGlyphBounds :: Semigroup GlyphBounds where
  append (GlyphBounds a) (GlyphBounds b) = GlyphBounds (a || b)

instance monoidGlyphBounds :: Monoid GlyphBounds where
  mempty = GlyphBounds $ const false

glyphBoundsNat :: GlyphF ~> Writer GlyphBounds
glyphBoundsNat (Circle p r a) = do
  tell $ GlyphBounds \p' -> let x' = p'.x - p.x
                                y' = p'.y - p.y
                            in Math.sqrt ((x' * x') + (y' * y')) < r
  pure a
glyphBoundsNat (Line _ _ a) = pure a
glyphBoundsNat (Rect p1 p2 a) = do
  tell $ GlyphBounds \p' -> (p'.x > p1.x && p'.x < p2.x) &&
                            (p'.y > p1.y && p'.y < p2.y)
  pure a
glyphBoundsNat (Stroke _ a) = pure a
glyphBoundsNat (Fill _ a) = pure a
glyphBoundsNat (Path _ a) = pure a


glyphBounds :: forall a. Glyph a -> GlyphBounds
glyphBounds = execWriter <<< foldFree glyphBoundsNat


clickGlyphs :: forall f a.
               Filterable f
            => f (Glyph a)
            -> Point
            -> f (Glyph a)
clickGlyphs gs p = filter pred gs
  where pred = \g -> unwrap (glyphBounds g) p
foreign import subscribeM :: forall m eff a r.
                             MonadEff (frp :: FRP | eff) m
                          => Event a
                          -> (a -> m r)
                          -> m (Eff (frp :: FRP | eff) Unit)
browser :: Aff _ Unit
browser = do
  canvas <- liftEff $ unsafePartial $ fromJust <$> getCanvasElementById "canvas"
  ctx <- liftEff $ getContext2D canvas

  {w} <- liftEff $ getScreenSize
  h <- liftEff $ getCanvasHeight canvas
  _ <- liftEff $ setCanvasWidth (w-2.0) canvas
  backCanvas <- liftEff $ newCanvas {w,h}

  let minView = Bp 200000.0
      maxView = Bp 300000.0
      v = widthToView w { min: minView, max: maxView }
      f :: View -> Fetch _
      f = fileFetch "./gwas.json"
      -- f = fetchWithView 100


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
      viewB = viewBehavior updateViews v


  -- x <- liftEff $ unsafeCoerceEff $ FRP.subscribe cDrag $ case _ of
  x <- liftEff $ unsafeCoerceEff $ subscribeM cDrag $ case _ of
    Left _      -> pure unit
    Right {x,y} -> scrollCanvas backCanvas canvas {x: -x, y: 0.0}

  -- _ <- liftEff $ unsafeCoerceEff $ FRP.subscribe viewB \v' -> do
  _ <- liftEff $ unsafeCoerceEff $ subscribeM viewB \v' -> do
    -- fetch & draw
    clearCanvas canvas
    _ <- launchAff $ fetchToCanvas h f v' ctx
    -- update other UI elements
    setViewUI $ "View range: "
             <> show (round $ unwrap v'.min) <> " - "
             <> show (round $ unwrap v'.max)

  _ <- forkAff do
    liftEff $ log "started kill thread"
    delay $ (Milliseconds 3000.0)
    liftEff $ log "Killing canvas drag subscriber!"
    liftEff $ unsafeCoerceEff x
    liftEff $ log "Killed canvas drag subscriber!"

  -- render first frame
  fetchToCanvas h f v ctx



main :: Eff _ _
main = launchAff browser
