module Genetics.Browser.Track where

import Prelude

import Control.Monad.Eff (Eff)
import Genetics.Browser.DataSource (DataSource)
import Genetics.Browser.Types (Bp(..), Chr, Point)
import Genetics.Browser.View (View, Range, Pixels)


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
           , vScale :: Pixels
           , height :: Pixels })


-- A Renderer maps data attached to a chromosome id to
-- a way to render said data,
-- when provided some extra information on where and how to render it
type Renderer eff a = { chrId :: ChrId, features :: Array a }
                   -> RenderParams -> Eff eff Unit


-- Commands the renderer thread can receive
data RenderCmds a =
    Render (Array { chrId :: ChrId, features :: Array a})
  | Scroll Pixels


type RenderBackend = { canvas :: CanvasElement
                     , backCanvas :: CanvasElement
                     }

renderer :: forall eff a.
            RenderBackend
         -> Renderer eff a
         -> AVar (RenderCmds a)
         -> Aff _ Unit
renderer cvs r cmds = forever do
  cmd <- takeVar cmds
  case cmd of
    Render fs -> do
      let toRender :: Array (RenderParams -> Eff _ Unit)
          toRender = map r fs
      -- Use current (or provided?) View to derive RenderParams for each
      pure unit

    Scroll n -> do
      pure unit



fetchTrack :: forall r.
              Track _ _ a
           -> Range r
           -> Aff _ (Array a)
fetchTrack track r = track.source.fetch r.lHand r.rHand



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


-- a `Track` is the more or less abstract representation of the track;
-- it needs to be run to do anything.

-- TODO it also needs to be able to receive messages and send messages

runTrack :: forall a r.
            View
         -> Track _ _ a
         -> Aff _ Unit
runTrack opts { fetch, render, getPoint, chrSize } = do
  ctx <- liftEff $ getContext2D opts.canvas

  {w,h} <- liftEff do
    {w} <- getScreenSize
    h <- getCanvasHeight opts.canvas
    _ <- setCanvasWidth (w-2.0) opts.canvas
    pure {w, h}

  trackState <- makeVar

  -- TODO handle resize
  let v = View.fromCanvasWidth chrSize opts

  feats <- fetch


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
