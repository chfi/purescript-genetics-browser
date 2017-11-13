module Genetics.Browser.Track where

import Prelude

import Genetics.Browser.View (View, Range, Pixels)


type Track aff eff a = { fetch :: (forall r. Range r -> Aff aff (Array a))
                       , render :: Array a -> Eff eff Unit
                       , getPoint :: Point -> Array a
                       , chrSize :: Chr -> Bp
                       }



-- type View r = { leftHand  :: { chr :: Chr, pos :: Bp }
--               , rightHand :: { chr :: Chr, pos :: Bp }
--               | r
--               }

type TrackDOM r = { canvas :: CanvasElement
                  , backCanvas :: CanvasElement
                  | r
                  }


fetchTrack :: forall r.
              Track _ _ a
           -> Range r
           -> Aff _ (Array a)
fetchTrack track r = track.fetch r



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

              -- NOTE `chrSizes` will probably be used other places. Reader?
-- TODO use RWS or something to clean up state etc.
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
