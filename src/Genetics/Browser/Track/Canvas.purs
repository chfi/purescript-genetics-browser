-- | This module provides a HTML5 canvas interface for the genetics browser,
-- | which wraps and optimizes all rendering calls

module Genetics.Browser.Track.UI.Canvas where


import Prelude

import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Uncurried (EffFn3, EffFn4, runEffFn3, runEffFn4)
import DOM.Node.Types (Element)
import Data.Bitraversable (bitraverse, bitraverse_)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Lens (iso, view, (^.))
import Data.Lens.Iso (Iso')
import Data.List (List)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(Tuple), uncurry)
import Genetics.Browser.Track.Backend (BatchGlyph, RenderedTrack, SingleGlyph, UISlots)
import Genetics.Browser.Types (Point)
import Genetics.Browser.Types.Coordinates (CoordSysView(..), ViewScale(..), viewScale)
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing)
import Graphics.Drawing as Drawing
import Unsafe.Coerce (unsafeCoerce)


_Element :: Iso' CanvasElement Element
_Element = iso unsafeCoerce unsafeCoerce

-- | Create a new CanvasElement, not attached to the DOM, with the provided String as its CSS class
foreign import createCanvas :: forall eff.
                               { width :: Number, height :: Number }
                            -> String
                            -> Eff eff CanvasElement


foreign import setElementStyleImpl :: forall e.
                                      EffFn3 e
                                      Element String String Unit

setElementStyle :: Element
                -> String
                -> String
                -> Eff _ Unit
setElementStyle = runEffFn3 setElementStyleImpl

setElementStyles :: Element -> Array (Tuple String String) -> Eff _ Unit
setElementStyles el =
  traverse_ (uncurry $ setElementStyle el)

setCanvasStyles :: CanvasElement -> Array (Tuple String String) -> Eff _ Unit
setCanvasStyles = setElementStyles <<< view _Element


setCanvasStyle :: CanvasElement
               -> String
               -> String
               -> Eff _ Unit
setCanvasStyle ce = setElementStyle (ce ^. _Element)

setCanvasZIndex :: CanvasElement -> Int -> Eff _ Unit
setCanvasZIndex ce i = setCanvasStyle ce "z-index" (show i)

setCanvasPosition :: forall r.
                     { left :: Number, top :: Number | r }
                  -> CanvasElement
                  -> Eff _ Unit
setCanvasPosition {left, top} ce =
  setCanvasStyles ce
    [ Tuple "position" "absolute"
    , Tuple "top"  (show top  <> "px")
    , Tuple "left" (show left <> "px") ]


foreign import appendCanvasElem :: forall e.
                                   Element
                                -> CanvasElement
                                -> Eff e Unit


-- | Sets some of the browser container's CSS to reasonable defaults
foreign import setContainerStyle :: forall e. Element -> Canvas.Dimensions -> Eff e Unit


foreign import drawCopies :: forall eff a.
                             EffFn4 eff
                             CanvasElement
                             { width :: Number, height :: Number }
                             Context2D
                             (Array Point)
                             Unit


foreign import setCanvasTranslation :: forall e.
                                       Point
                                    -> CanvasElement
                                    -> Eff e Unit


foreign import scrollCanvas :: forall eff.
                               CanvasElement
                            -> CanvasElement
                            -> Point
                            -> Eff eff Unit


foreign import canvasDragImpl :: forall eff.
                                 CanvasElement
                              -> ( { during :: Nullable Point
                                   , total :: Nullable Point } -> Eff eff Unit )
                              -> Eff eff Unit


foreign import canvasWheelCBImpl :: forall eff.
                                    CanvasElement
                                 -> (Number -> Eff eff Unit)
                                 -> Eff eff Unit




canvasDrag :: (Either Point Point -> Eff _ Unit)
           -> CanvasElement
           -> Eff _ Unit
canvasDrag f el =
  let toEither g {during, total} = case toMaybe during of
        Just p  -> g $ Right p
        Nothing -> g $ Left $ fromMaybe {x:zero, y:zero} $ toMaybe total
  in canvasDragImpl el (toEither f)

{-
dragScroll :: Number
           -> BrowserCanvas
           -> AVar UpdateView
           -> Eff _ Unit
dragScroll width cnv av = canvasDrag f cnv.overlay
  where f = case _ of
              Left  {x,y} -> queueCmd av $ ScrollView $ (-x) / width
              Right {x,y} -> scrollCanvas cnv.buffer cnv.track {x: -x, y: zero}

wheelZoom :: Number
          -> AVar UpdateView
          -> CanvasElement
          -> Eff _ Unit
wheelZoom scale av cv =
  canvasWheelCBImpl cv \dY ->
    queueCmd av $ ZoomView $ 1.0 + (scale * dY)
-}

-- TODO browser background color shouldn't be hardcoded
backgroundColor :: String
backgroundColor = "white"


-- TODO the back canvas should have the option of being bigger than
-- the front, to minimize redrawing
-- needs adding the offset of the front canvas, and validating that back >= front
newtype BufferedCanvas =
  BufferedCanvas { back  :: CanvasElement
                 , front :: CanvasElement
                 }

derive instance newtypeBufferedCanvas :: Newtype BufferedCanvas _

bufferedCanvas :: Canvas.Dimensions
               -> Eff _ BufferedCanvas
bufferedCanvas dim = do
  back  <- createCanvas dim "buffer"
  front <- createCanvas dim "front"
  let bc = BufferedCanvas { back, front }
  blankBuffer bc
  pure bc


setBufferedCanvasSize :: Canvas.Dimensions
                      -> BufferedCanvas
                      -> Eff _ Unit
setBufferedCanvasSize dim bc@(BufferedCanvas {back, front}) = do
  _ <- Canvas.setCanvasDimensions dim back
  _ <- Canvas.setCanvasDimensions dim front
  blankBuffer bc
  pure unit


translateBuffer :: Point
                -> BufferedCanvas
                -> Eff _ Unit
translateBuffer p (BufferedCanvas bc) = do
  setCanvasTranslation p bc.back
  setCanvasTranslation p bc.front


drawToBuffer :: BufferedCanvas
             -> (Context2D -> Eff _ Unit)
             -> Eff _ Unit
drawToBuffer (BufferedCanvas {back}) f = do
  ctx <- Canvas.getContext2D back
  f ctx

blankBuffer :: BufferedCanvas
            -> Eff _ Unit
blankBuffer (BufferedCanvas {back}) = do
  backCtx <- Canvas.getContext2D back
  {width, height} <- Canvas.getCanvasDimensions back
  _ <- Canvas.setFillStyle backgroundColor backCtx
  _ <- Canvas.fillRect backCtx { x: 0.0, y: 0.0, w: width, h: height }
  pure unit


flipBuffer :: BufferedCanvas
           -> Eff _ Unit
flipBuffer (BufferedCanvas {back, front}) = do
-- NOTE this assumes back and front are the same size
  frontCtx <- Canvas.getContext2D front
  let imgSrc = Canvas.canvasElementToImageSource back

  {width, height} <- Canvas.getCanvasDimensions front

  _ <- Canvas.fillRect frontCtx { x: 0.0, y: 0.0, w: width, h: height }
  _ <- Canvas.drawImage frontCtx imgSrc 0.0 0.0
  pure unit




-- | The `width` & `height` of the TrackCanvas is the area glyphs render to;
-- | the browser shows a `width` pixels slice of the whole coordinate system.
-- | `glyphBuffer` is what individual glyphs can be rendered to and copied from, for speed.
newtype TrackCanvas =
  TrackCanvas { canvas :: BufferedCanvas
              , width :: Number
              , height :: Number
              , glyphBuffer  :: CanvasElement
              }

derive instance newtypeTrackCanvas :: Newtype TrackCanvas _

glyphBufferSize :: Canvas.Dimensions
glyphBufferSize = { width: 100.0, height: 100.0 }

trackCanvas :: Canvas.Dimensions
            -> Eff _ TrackCanvas
trackCanvas dim = do
  canvas <- bufferedCanvas dim
  glyphBuffer <- createCanvas glyphBufferSize "glyphBuffer"

  pure $ TrackCanvas { width: dim.width
                     , height: dim.height
                     , canvas, glyphBuffer }

setTrackCanvasSize :: Canvas.Dimensions
                   -> TrackCanvas
                   -> Eff _ TrackCanvas
setTrackCanvasSize dim (TrackCanvas tc) = do
  setBufferedCanvasSize dim tc.canvas
  pure $ TrackCanvas
    $ tc { width = dim.width, height = dim.height }



type TrackPadding =
  { left :: Number, right :: Number
  , top :: Number, bottom :: Number }

-- | A `BrowserCanvas` consists of a double-buffered `track`
-- | which is what the genetics browser view is rendered onto,
-- | and a transparent `overlay` canvas the UI is rendered onto.

-- | The `dimensions`, `trackPadding`, and track.width/height are
-- | related such that
-- | track.width + horizontal.left + horizontal.right = dimensions.width
-- | track.height + vertical.top + vertical.bottom = dimensions.height
newtype BrowserCanvas =
  BrowserCanvas { track        :: TrackCanvas
                , trackPadding :: TrackPadding
                , dimensions   :: Canvas.Dimensions
                , overlay      :: CanvasElement
                }


derive instance newtypeBrowserCanvas :: Newtype BrowserCanvas _


foreign import debugBrowserCanvas :: forall e.
                                     String
                                  -> BrowserCanvas
                                  -> Eff e Unit


subtractPadding :: Canvas.Dimensions
                -> TrackPadding
                -> Canvas.Dimensions
subtractPadding {width, height} pad =
  { width:  width - pad.left - pad.right
  , height: height - pad.top - pad.bottom }

uiSlots :: BrowserCanvas
        -> UISlots
uiSlots (BrowserCanvas bc) =
  let track   = subtractPadding bc.dimensions bc.trackPadding
      overlay = bc.dimensions
      pad     = bc.trackPadding
  in { left:   { offset: { x: 0.0, y: pad.top }
               , size:   { height: track.height, width: pad.left }}
     , right:  { offset: { x: overlay.width - pad.right, y: pad.top }
               , size:   { height: track.height, width: pad.right }}
     , top:    { offset: { x: pad.left, y: 0.0 }
               , size:   { height: pad.top, width: track.width }}
     , bottom: { offset: { x: overlay.width  - pad.right
                         , y: overlay.height - pad.bottom }
               , size:   { height: pad.bottom, width: track.width }}
    }


setBrowserCanvasSize :: Canvas.Dimensions
                     -> BrowserCanvas
                     -> Eff _ BrowserCanvas
setBrowserCanvasSize dim (BrowserCanvas bc) = do

  let trackDim = subtractPadding dim bc.trackPadding

  track <- setTrackCanvasSize trackDim bc.track
  _ <- Canvas.setCanvasDimensions dim bc.overlay

  pure $ BrowserCanvas
       $ bc { dimensions = dim
            , track = track }




-- | Creates a `BrowserCanvas` and appends it to the provided element.
-- | Resizes the container element to fit.
browserCanvas :: Canvas.Dimensions
              -> TrackPadding
              -> Element
              -> Eff _ BrowserCanvas
browserCanvas dimensions trackPadding el = do

  setContainerStyle el dimensions

  let trackDim = subtractPadding dimensions trackPadding
  track   <- trackCanvas trackDim
  overlay <- createCanvas dimensions "overlay"

  let trackEl = _.front  $ unwrap
                $ _.canvas $ unwrap $ track

  setCanvasZIndex trackEl 1
  setCanvasZIndex overlay 2

  setCanvasPosition trackPadding trackEl
  setCanvasPosition {left: 0.0, top: 0.0} overlay

  appendCanvasElem el trackEl
  appendCanvasElem el overlay

  pure $ BrowserCanvas { track, trackPadding
                       , overlay, dimensions }


trackViewScale :: BrowserCanvas
               -> CoordSysView
               -> ViewScale
trackViewScale (BrowserCanvas bc) = viewScale (unwrap $ bc.track)


renderSingleGlyphs :: TrackCanvas
                   -> Array SingleGlyph
                   -> Eff _ Unit
renderSingleGlyphs (TrackCanvas tc) glyphs = do
  let draw ctx = foreachE glyphs \s -> do
        Drawing.render ctx
          $ Drawing.translate s.point.x s.point.y
          $ s.drawing unit

  -- drawToBuffer tc.canvas draw

  ctx <- Canvas.getContext2D (unwrap tc.canvas).front
  draw ctx


renderBatchGlyphs :: TrackCanvas
                  -> BatchGlyph Point
                  -> Eff _ Unit
renderBatchGlyphs (TrackCanvas tc) {drawing, points} = do
  glyphBfr <- Canvas.getContext2D tc.glyphBuffer

  let x0 = glyphBufferSize.width  / 2.0
      y0 = glyphBufferSize.height / 2.0

  Drawing.render glyphBfr
    $ Drawing.translate x0 y0 drawing

  ctx <- Canvas.getContext2D (unwrap tc.canvas).front

  runEffFn4 drawCopies tc.glyphBuffer glyphBufferSize ctx points


renderGlyphs :: TrackCanvas
             -> Number
             -> Array RenderedTrack
             -> Eff _ Unit
renderGlyphs track offset rts = do
  translateBuffer {x: offset, y: zero} (unwrap track).canvas
  for_ rts $ either
    (renderBatchGlyphs track)
    (renderSingleGlyphs track)


renderBrowser :: Milliseconds
              -> BrowserCanvas
              -> Number
              -> { tracks     :: List (Array RenderedTrack)
                 , relativeUI :: Drawing
                 , fixedUI :: Drawing }
              -> Aff _ _
renderBrowser d (BrowserCanvas bc) offset ui = do

  let bfr = (unwrap bc.track).canvas
      cnv = (unwrap bfr).front

  -- liftEff $ blankBuffer bfr

  -- backCtx <- Canvas.getContext2D back
  -- translateBuffer {x: zero, y: zero} bc
  -- setCanvasTranslation {x: 0.0, y: 0.0 } back

  ctx <- liftEff $ Canvas.getContext2D cnv
  liftEff do
    {width, height} <- Canvas.getCanvasDimensions cnv
    _ <- Canvas.setFillStyle backgroundColor ctx
    translateBuffer {x: zero, y: zero} bfr
    void $ Canvas.fillRect ctx { x: 0.0, y: 0.0, w: width, h: height }

  liftEff $ translateBuffer {x: (-offset), y: zero} bfr

  for_ ui.tracks \t -> do
      liftEff $ for_ t $ either
        (renderBatchGlyphs  bc.track)
        (renderSingleGlyphs bc.track)

      -- liftEff $ flipBuffer bfr
      delay d

  liftEff do

    overlayCtx <- Canvas.getContext2D bc.overlay
    Drawing.render overlayCtx ui.fixedUI

    Drawing.render ctx ui.relativeUI

  -- drawToBuffer bfr \tCtx -> Drawing.render tCtx $ ui.relativeUI
  -- liftEff $ flipBuffer bfr

flipTrack :: BrowserCanvas
          -> Eff _ Unit
flipTrack (BrowserCanvas bc) = do
  let (TrackCanvas tc) = bc.track
      buffered = tc.canvas

  flipBuffer buffered

blankTrack :: BrowserCanvas
           -> Eff _ Unit
blankTrack (BrowserCanvas bc) = do
  let (TrackCanvas tc) = bc.track
      buffered = tc.canvas

  blankBuffer buffered

renderTrack :: BrowserCanvas
            -> (TrackCanvas -> Eff _ Unit)
            -> Eff _ Unit
renderTrack (BrowserCanvas bc) f = f bc.track
drawOnTrack :: BrowserCanvas
            -> (Context2D -> Eff _ Unit)
            -> Eff _ Unit
drawOnTrack (BrowserCanvas bc) f = do
  let (TrackCanvas tc) = bc.track
      buffered = tc.canvas

  drawToBuffer buffered f
