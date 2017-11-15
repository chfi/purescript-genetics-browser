module Genetics.Browser.Track where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, makeVar, takeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (StateT(..))
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Genetics.Browser.DataSource (DataSource)
import Genetics.Browser.Glyph (Glyph, circle)
import Genetics.Browser.GlyphF.Canvas (renderGlyph)
import Genetics.Browser.Types (Bp(..), BpPerPixel(..), Chr, ChrId(..), Point, Range, bpToPixels)
import Genetics.Browser.UI.Native (getScreenSize)
import Genetics.Browser.UI.Native.View as View
import Genetics.Browser.View (View, Pixels)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasHeight, getContext2D, setCanvasWidth)
import Graphics.Canvas as C


type Track aff eff a = { source :: DataSource aff a
                       , render :: Array a -> Eff eff Unit
                       , getPoint :: Point -> Array a
                       , chrSize :: Chr -> Bp
                       }

{-

Have a way to "prepare" data per-chromosome for rendering.

Then have a way to actually render that prepared data,
given some extra information such as offset, height scaling, etc.



Also need to map clicks backward, but that should be doable.



Maybe render to an offscreen canvas and copyImage.
Not sure that makes sense either
-}


newtype RenderParams =
  RenderParams
    (Maybe { xOffset :: Pixels
           , yOffset :: Pixels
           , height :: Pixels })



-- A Renderer maps data attached to a chromosome id to
-- a way to render said data,
-- when provided some extra information on where and how to render it
type Renderer eff a = { chrId :: ChrId, features :: Array a }
                   -> RenderParams -> Eff eff Unit

-- I get the feeling using all of these renderers is going to be a hylomorphism...

-- the center of a glyph is assumed to be at {0, 0},
-- glyphs are be placed by translating the canvas
render1 :: Context2D
        -> Glyph Unit
        -> RenderParams
        -> Eff _ Unit
render1 _ g   (RenderParams  Nothing) = pure unit
render1 ctx g (RenderParams (Just p)) = C.withContext ctx do
  -- translate by p.x,y
  _ <- C.translate { translateX: p.xOffset
                   , translateY: p.yOffset } ctx

  -- translate by height
  _ <- C.translate { translateX: 0.0
                   , translateY: p.height } ctx

  renderGlyph ctx g


-- given a way to create a glyph from a feature,
-- and a way to create a RenderParam offset from a feature,
-- we create a function that takes a bunch of features,
-- a chr-wide renderparam,
-- and renders a whole chromosome.
render1Chr :: forall a.
              Context2D
           -> (a -> { glyph :: Glyph Unit, rp :: RenderParams } )
           -> Array a
           -> RenderParams
           -> Eff _ Unit
render1Chr ctx _ _ (RenderParams Nothing) = pure unit
render1Chr ctx toG features (RenderParams (Just p)) = C.withContext ctx do
  -- translate to Chr position
  _ <- C.translate { translateX: p.xOffset
                   , translateY: p.yOffset } ctx

  for_ features (\a -> let {glyph, rp} = toG a
                       in render1 ctx glyph rp)


-- given an array of chromosomes that can be constructed,
-- and a way to create a render offset from a chrId,
-- we can render a bunch of chrs at once.
renderManyChrs :: forall a.
                  Context2D
               -> (ChrId -> RenderParams)
               -> Array { chrId :: ChrId
                        , chrRenderer :: RenderParams -> Eff _ Unit }
               -> RenderParams -> Eff _ Unit
renderManyChrs ctx toRP chrs ( RenderParams Nothing) = pure unit
renderManyChrs ctx toRP chrs (RenderParams (Just p)) = C.withContext ctx do
  _ <- C.translate { translateX: p.xOffset
                   , translateY: p.yOffset } ctx

  for_ chrs (\{chrId, chrRenderer} -> chrRenderer (toRP chrId))




chrRenderParams :: { xOffset :: Number, xDist :: Number }
                -> (ChrId -> Number)
                -> ChrId
                -> RenderParams
chrRenderParams { xOffset, xDist } chrIndex chrId = RenderParams $
  Just { xOffset: xOffset+xDist*(chrIndex chrId), yOffset: 0.0, height: 0.0 }



posInPixels :: Bp
            -> Bp
            -> BpPerPixel
            -> Pixels
posInPixels pos size scale = bpToPixels scale (pos / size)



placeGwas :: forall a r.
             Bp
          -> BpPerPixel
          -> { pos :: Bp, height :: Pixels }
          -> a
          -> { glyph :: a, rp :: RenderParams }
placeGwas size scale { pos, height } glyph = { glyph, rp }
  where rp = RenderParams $ Just { xOffset: posInPixels pos size scale
                                 , yOffset: 0.0
                                 , height
                                 }



renderGwas :: Array (Tuple ChrId { pos :: Bp, height :: Pixels } )
           -> Context2D
           -> Eff _ Unit
renderGwas arr ctx = do
  let toG = const $ circle {x: 0.0, y: 0.0} 3.0

  pure unit




-- Commands the renderer thread can receive
data RenderCmds a =
    Render (Array { chrId :: ChrId, features :: Array a})
  | SetView View
  | Scroll Pixels




type RenderBackend = { canvas :: CanvasElement
                     , backCanvas :: CanvasElement
                     }

renderer :: forall eff a.
            RenderBackend
         -> Renderer eff a
         -> AVar (RenderCmds a)
         -> StateT View (Aff _) Unit
renderer cvs r cmdVar = forever do
  cmd <- lift $ takeVar cmdVar
  case cmd of
    Render fs -> do
      let toRender :: Array (RenderParams -> Eff _ Unit)
          toRender = map r fs
      -- Use current (or provided?) View to derive RenderParams for each
      pure unit

    Scroll n -> do
      pure unit

    SetView v -> do
      State.put v
      pure unit



fetchTrack :: forall r a.
              Track _ _ a
           -> Range r
           -> Aff _ (Array a)
fetchTrack track r = track.source.fetch r.lHand r.rHand



{-
trackEvents :: _
trackEvents = viewB
  where cDrag = canvasDrag canvas
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
-}


-- a `Track` is the more or less abstract representation of the track;
-- it needs to be run to do anything.

-- TODO it also needs to be able to receive messages and send messages

runTrack :: forall a r.
            View
         -> Track _ _ a
         -> Aff _ Unit
runTrack opts { source, render, getPoint, chrSize } = do
  -- ctx <- liftEff $ getContext2D opts.canvas

  -- {w,h} <- liftEff do
  --   {w} <- getScreenSize
  --   h <- getCanvasHeight opts.canvas
  --   _ <- setCanvasWidth (w-2.0) opts.canvas
  --   pure {w, h}

  -- trackState <- makeVar

  -- -- TODO handle resize
  -- let v = View.fromCanvasWidth chrSize opts

  -- feats <- fetch
  pure unit

{-

browser :: Aff _ Unit
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

  let minView = Bp 0.0
      maxView = Bp 15000000.0
      v = View.fromCanvasWidth w { min: minView, max: maxView }
      f :: View -> Fetch _
      f = fileFetch uri

  fs <- f v


  let renderer :: View -> Array _ -> _
      renderer = renderGlyphs (browserTransform h) ctx
      fetch :: View -> Aff _ _
      fetch v = do

        let gs = glyphifyFeatures fs v
            annGs = zip fs gs
        let gs' = filterFeatures v annGs

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

  fetch v
  -}
