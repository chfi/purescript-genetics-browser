-- | This module provides a HTML5 canvas interface for the genetics browser,
-- | which wraps and optimizes all rendering calls

module Genetics.Browser.Track.UI.Canvas
       ( BrowserCanvas
       , browserCanvas
       ) where


import Prelude

import Color (black)
import Control.Coroutine (Consumer, Producer, connect, runProcess)
import Control.Coroutine as Co
import Control.Monad.Aff (Aff, Fiber, Milliseconds, delay, finally, forkAff, killFiber, launchAff, launchAff_)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, makeVar, putVar, readVar, takeVar, tryTakeVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn4, runEffFn2, runEffFn4)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (forever)
import DOM.Classy.Node (toNode)
import DOM.Classy.ParentNode (toParentNode)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Element as DOM
import DOM.Node.Node (appendChild) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import DOM.Node.Types (Element, Node)
import Data.Argonaut (Json, _Number, _Object, _String)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldMap, for_, intercalate, null)
import Data.Foreign (Foreign, F)
import Data.Lens (to, united, (^.), (^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Pair (Pair(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(Tuple))
import Genetics.Browser.Track.Backend (Padding, RenderedTrack, browser, bumpFeatures, zipMapsWith)
import Genetics.Browser.Track.Demo (Annot, BedFeature, GWASFeature, annotLegendTest, demoTracks, getAnnotations, getGWAS, getGenes, produceAnnots, produceGWAS, produceGenes)
import Genetics.Browser.Types (Bp(..), ChrId(ChrId), Point)
import Genetics.Browser.Types.Coordinates (CoordSys(..), _TotalSize, coordSys, pairSize, scalePairBy, translatePairBy)
import Graphics.Canvas (CanvasElement, Context2D, getContext2D)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, fillColor, filled, rectangle, white)
import Graphics.Drawing as Drawing
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Simple.JSON (readJSON)
import Unsafe.Coerce (unsafeCoerce)


-- | Create a new CanvasElement, not attached to the DOM
foreign import createCanvas :: forall eff.
                               { width :: Number, height :: Number }
                            -> Eff eff CanvasElement


foreign import setCanvasZIndex :: forall eff.
                                  Int
                               -> CanvasElement
                               -> Eff eff Unit


foreign import appendCanvasElem :: forall e.
                                   Element
                                -> CanvasElement
                                -> Eff e Unit




-- | Sets some of the browser container's CSS to reasonable defaults
foreign import setContainerStyle :: forall e. Element -> Eff e Unit

-- | Sets the provided element's CSS `width` and `height` accordingly
foreign import setElementSize :: forall e.
                                 Canvas.Dimensions
                              -> Element
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
  back  <- createCanvas dim
  front <- createCanvas dim
  pure $ BufferedCanvas { back, front }


setBufferedCanvasSize :: Canvas.Dimensions
                      -> BufferedCanvas
                      -> Eff _ Unit
setBufferedCanvasSize dim (BufferedCanvas {back, front}) = do
  _ <- Canvas.setCanvasDimensions dim back
  _ <- Canvas.setCanvasDimensions dim front
  pure unit


  -- DOM.setId (wrap "buffer")  (element buffer)

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
  glyphBuffer <- createCanvas glyphBufferSize

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
                -- , container    :: Element
                }



subtractPadding :: Canvas.Dimensions
                -> TrackPadding
                -> Canvas.Dimensions
subtractPadding {width, height} pad =
  { width:  width - pad.left - pad.right
  , height: height - pad.top - pad.bottom }

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

  setContainerStyle el
  setElementSize dimensions el

  let trackDim = subtractPadding dimensions trackPadding
  track   <- trackCanvas trackDim
  overlay <- createCanvas dimensions

  let trackEl = _.front  $ unwrap
                $ _.canvas $ unwrap $ track

  setCanvasZIndex 1 trackEl
  setCanvasZIndex 2 overlay

  appendCanvasElem el trackEl
  appendCanvasElem el overlay

  pure $ BrowserCanvas { track, trackPadding
                       , overlay, dimensions }
