-- | This module provides a HTML5 canvas interface for the genetics browser,
-- | which wraps and optimizes all rendering calls

module Genetics.Browser.Canvas where


import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, EffFn4, runEffFn2, runEffFn3, runEffFn4)
import DOM.Node.Types (Element)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, foldl, for_, length)
import Data.Int as Int
import Data.Lens (Getter', Lens', Prism', iso, prism', to, view, (^.), (^?))
import Data.Lens.Iso (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record as Lens
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds)
import Data.Traversable (traverse, traverse_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Variant (Variant, case_, onMatch)
import Debug.Trace as Debug
import Genetics.Browser.Layer (BrowserDimensions, BrowserPadding, BrowserSlots, Component(Padded, Full), Layer(Layer), LayerType(Scrolling, Fixed), _Component, browserSlots, setContextTranslation, slotContext)
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, Point)
import Graphics.Drawing (render, translate) as Drawing
import Graphics.Drawing.Font (font, fontString) as Drawing
import Graphics.Drawing.Font (sansSerif)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)


_Element :: Iso' CanvasElement Element
_Element = iso unsafeCoerce unsafeCoerce

-- | Create a new CanvasElement, not attached to the DOM, with the provided String as its CSS class
foreign import createCanvas :: ∀ eff.
                               { width :: Number, height :: Number }
                            -> String
                            -> Eff eff CanvasElement


foreign import setElementStyleImpl :: ∀ e.
                                      EffFn3 e
                                      Element String String
                                      Unit

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

setCanvasPosition :: ∀ r.
                     { left :: Number, top :: Number | r }
                  -> CanvasElement
                  -> Eff _ Unit
setCanvasPosition {left, top} ce =
  setCanvasStyles ce
    [ Tuple "position" "absolute"
    , Tuple "top"  (show top  <> "px")
    , Tuple "left" (show left <> "px") ]


foreign import appendCanvasElem :: ∀ e.
                                   Element
                                -> CanvasElement
                                -> Eff e Unit


-- | Sets some of the browser container's CSS to reasonable defaults
foreign import setContainerStyle :: ∀ e. Element -> Canvas.Dimensions -> Eff e Unit


foreign import drawCopies :: ∀ eff a.
                             EffFn4 eff
                             CanvasElement Canvas.Dimensions Context2D (Array Point)
                             Unit


foreign import setCanvasTranslation :: ∀ e.
                                       Point
                                    -> CanvasElement
                                    -> Eff e Unit


foreign import canvasClickImpl :: ∀ e.
                                  EffFn2 e
                                  CanvasElement (Point -> Eff e Unit)
                                  Unit


canvasClick :: CanvasElement -> (Point -> Eff _ Unit) -> Eff _ Unit
canvasClick = runEffFn2 canvasClickImpl


-- | Attaches two callbacks to the BrowserCanvas click event handler,
-- | provided with the track canvas and static overlay canvas' relative
-- | coordinates of the click, respectively.
browserOnClick :: BrowserCanvas
               -> { track   :: Point -> Eff _ Unit
                  , overlay :: Point -> Eff _ Unit }
               -> Eff _ Unit
browserOnClick (BrowserCanvas bc) {track, overlay} =
  canvasClick bc.staticOverlay \o -> do
    let t = { x: o.x - bc.trackPadding.left
            , y: o.y - bc.trackPadding.top }
    overlay o
    track t

-- | Attaches a callback to a layer by name, if it exists
-- | The callback is a variant whose symbol describes which
-- | browser slot the coordinates are called with
-- | Returns `false` if the layer does not exist
browserClick :: Warn "TODO implement me!"
             => BrowserContainer
             -> String
             -> Variant (BrowserSlots (Point -> Eff _ Unit))
             -> Eff _ Boolean
browserClick (BrowserContainer bc) name cb = do
  layers <- Ref.readRef bc.layers
  case Map.lookup name layers of
    Nothing -> pure false
    Just l  -> do
      -- TODO implement me
      -- 0. precompose callback w/ `slotOffset` to transform coordinates
      -- 1. add callback to correct layer
      -- 2. ensure events bubble thru layers
      pure true



foreign import scrollCanvasImpl :: ∀ e.
                                   EffFn3 e
                                   CanvasElement CanvasElement Point
                                   Unit

scrollCanvas :: BufferedCanvas
             -> Point
             -> Eff _ Unit
scrollCanvas (BufferedCanvas bc) = runEffFn3 scrollCanvasImpl bc.back bc.front


scrollCanvas' :: BrowserContainer
              -> Point
              -> Eff _ Unit
scrollCanvas' (BrowserContainer bc) pt = do
  layers <- Ref.readRef bc.layers

  for_ layers $ \lc -> case lc ^? _Buffer of
    Nothing -> pure unit
    Just bc -> scrollCanvas bc pt


foreign import canvasDragImpl :: ∀ eff.
                                 CanvasElement
                              -> ( { during :: Nullable Point
                                   , total :: Nullable Point } -> Eff eff Unit )
                              -> Eff eff Unit


foreign import canvasWheelCBImpl :: ∀ eff.
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


-- | Takes a BrowserCanvas and a callback function that is called with the
-- | total dragged distance when a click & drag action is completed.
dragScroll :: BrowserCanvas
           -> (Point -> Eff _ Unit)
           -> Eff _ Unit
dragScroll (BrowserCanvas bc) cb = canvasDrag f bc.staticOverlay
  where f = case _ of
              Left  p     -> cb p
              Right {x} -> do
                let p' = {x: -x, y: 0.0}
                scrollCanvas (_.canvas $ unwrap bc.track) p'
                scrollCanvas bc.trackOverlay p'

dragScroll' :: BrowserContainer
            -> (Point -> Eff _ Unit)
            -> Eff _ Unit
dragScroll' bc'@(BrowserContainer {element}) cb =
  canvasDrag f (unsafeCoerce element) -- actually safe
  where f = case _ of
              Left  p    -> cb p
              Right {x} -> do
                let p' = {x: -x, y: 0.0}
                scrollCanvas' bc' p'


-- | Takes a BrowserCanvas and a callback function that is called with each
-- | wheel scroll `deltaY`. Callback is provided with only the sign of `deltaY`
-- | as to be `deltaMode` agnostic.
wheelZoom :: BrowserCanvas
          -> (Number -> Eff _ Unit)
          -> Eff _ Unit
wheelZoom (BrowserCanvas bc) cb =
  canvasWheelCBImpl bc.staticOverlay cb


wheelZoom' :: BrowserContainer
          -> (Number -> Eff _ Unit)
          -> Eff _ Unit
wheelZoom' bc cb = Debug.trace "unimplemented!" \_ -> pure unit
  -- canvasWheelCBImpl bc.staticOverlay cb


-- | Helper function for erasing the contents of a canvas context given its dimensions
clearCanvas :: Context2D -> Canvas.Dimensions -> Eff _ Unit
clearCanvas ctx {width, height} =
  void $ Canvas.clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }


blankLayer :: Warn "TODO: TEST!"
           => LayerCanvas
           -> Eff _ Unit
blankLayer = case _ of
  Buffer bc -> blankBuffer bc
  Static el -> do
    ctx <- Canvas.getContext2D el
    Canvas.withContext ctx do
      setCanvasTranslation {x: zero, y: zero} el
      dims <- Canvas.getCanvasDimensions el
      clearCanvas ctx dims




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

createBufferedCanvas :: Canvas.Dimensions
                     -> String
                     -> Eff _ BufferedCanvas
createBufferedCanvas dim name = do
  back  <- createCanvas dim $ name <> "-buffer"
  front <- createCanvas dim $ name
  let bc = BufferedCanvas { back, front }
  blankBuffer bc
  pure bc

getBufferedContext :: BufferedCanvas
                   -> Eff _ Context2D
getBufferedContext = Canvas.getContext2D <$> _.front <<< unwrap

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

drawToLayer :: Warn "TODO: TEST!"
            => LayerCanvas
            -> (Context2D -> Eff _ Unit)
            -> Eff _ Unit
drawToLayer lc f =
  case lc of
    Static c -> f =<< Canvas.getContext2D c
    Buffer b -> drawToBuffer b f

blankBuffer :: BufferedCanvas
            -> Eff _ Unit
blankBuffer bc@(BufferedCanvas {back, front}) = do
  translateBuffer {x: zero, y: zero} bc

  dim <- Canvas.getCanvasDimensions back

  for_ [back, front]
    $ flip clearCanvas dim <=< Canvas.getContext2D

flipBuffer :: BufferedCanvas
           -> Eff _ Unit
flipBuffer (BufferedCanvas {back, front}) = do
-- NOTE this assumes back and front are the same size
  frontCtx <- Canvas.getContext2D front
  let imgSrc = Canvas.canvasElementToImageSource back

  dim <- Canvas.getCanvasDimensions front

  clearCanvas frontCtx dim
  _ <- Canvas.drawImage frontCtx imgSrc 0.0 0.0
  pure unit




-- | The `width` & `height` of the TrackCanvas is the area glyphs render to;
-- | the browser shows a `width` pixels slice of the whole coordinate system.
-- | `glyphBuffer` is what individual glyphs can be rendered to and copied from, for speed.
newtype TrackCanvas =
  TrackCanvas { canvas      :: BufferedCanvas
              , dimensions  :: Canvas.Dimensions
              , glyphBuffer :: CanvasElement
              }

-- | using hardcoded inner padding for now; makes it look a bit better
trackInnerPad :: Number
trackInnerPad = 5.0

derive instance newtypeTrackCanvas :: Newtype TrackCanvas _

_Dimensions :: ∀ n r1 r2.
               Newtype n { dimensions :: Canvas.Dimensions | r1 }
            => Lens' n Canvas.Dimensions
_Dimensions = _Newtype <<< Lens.prop (SProxy :: SProxy "dimensions")

trackTotalDimensions :: Canvas.Dimensions -> Canvas.Dimensions
trackTotalDimensions d = { width:  d.width  + extra
                         , height: d.height + extra }
  where extra = 2.0 * trackInnerPad


trackCanvas :: Canvas.Dimensions
            -> Eff _ TrackCanvas
trackCanvas dim = do
  canvas <- createBufferedCanvas (trackTotalDimensions dim) "track"
  glyphBuffer <- createCanvas glyphBufferSize "glyphBuffer"

  pure $ TrackCanvas { dimensions: dim
                     , canvas, glyphBuffer }

setTrackCanvasSize :: Canvas.Dimensions
                   -> TrackCanvas
                   -> Eff _ TrackCanvas
setTrackCanvasSize dim (TrackCanvas tc) = do
  setBufferedCanvasSize (trackTotalDimensions dim) tc.canvas
  pure $ TrackCanvas
    $ tc { dimensions = dim }


-- TODO change name to BrowserPadding
type TrackPadding =
  { left :: Number, right :: Number
  , top :: Number, bottom :: Number }

-- | A `BrowserCanvas` consists of a double-buffered `track`
-- | which is what the genetics browser view is rendered onto,
-- | and a transparent `staticOverlay` canvas the UI is rendered onto,
-- | with a (also transparent) `trackOverlay` that scrolls with the track view.

-- | The `dimensions`, `trackPadding`, and track.width/height are
-- | related such that
-- | track.width + horizontal.left + horizontal.right = dimensions.width
-- | track.height + vertical.top + vertical.bottom = dimensions.height
newtype BrowserCanvas =
  BrowserCanvas { track         :: TrackCanvas
                , trackPadding  :: TrackPadding
                , dimensions    :: Canvas.Dimensions
                , staticOverlay :: CanvasElement
                , trackOverlay  :: BufferedCanvas
                , container :: Element
                }


derive instance newtypeBrowserCanvas :: Newtype BrowserCanvas _

newtype BrowserContainer =
  BrowserContainer { layers      :: Ref (Map String LayerCanvas)
                   , dimensions  :: Ref BrowserDimensions
                   , element     :: Element
                   , glyphBuffer :: CanvasElement}


derive instance newtypeBrowserContainer :: Newtype BrowserContainer _




getDimensions :: ∀ m.
                 MonadEff _ m
              => BrowserContainer
              -> m BrowserDimensions
getDimensions (BrowserContainer {dimensions}) =
  liftEff $ Ref.readRef dimensions


_Track :: Lens' BrowserCanvas TrackCanvas
_Track = _Newtype <<< Lens.prop (SProxy :: SProxy "track")


foreign import debugBrowserCanvas :: ∀ e.
                                     String
                                  -> BrowserCanvas
                                  -> Eff e Unit


subtractPadding :: Canvas.Dimensions
                -> TrackPadding
                -> Canvas.Dimensions
subtractPadding {width, height} pad =
  { width:  width - pad.left - pad.right
  , height: height - pad.top - pad.bottom }


type UISlot = { offset :: Point
              , size   :: Canvas.Dimensions }

data UISlotGravity = UILeft | UIRight | UITop | UIBottom

derive instance eqUISlotGravity :: Eq UISlotGravity
derive instance ordUISlotGravity :: Ord UISlotGravity

type UISlots = { left   :: UISlot
               , right  :: UISlot
               , top    :: UISlot
               , bottom :: UISlot }


uiSlots :: Warn "deprecated"
        => BrowserCanvas
        -> UISlots
uiSlots (BrowserCanvas bc) =
  let track   = subtractPadding bc.dimensions bc.trackPadding
      browser = bc.dimensions
      pad     = bc.trackPadding
  in { left:   { offset: { x: 0.0, y: pad.top }
               , size:   { height: track.height, width: pad.left }}
     , right:  { offset: { x: browser.width - pad.right, y: pad.top }
               , size:   { height: track.height, width: pad.right }}
     , top:    { offset: { x: pad.left, y: 0.0 }
               , size:   { height: pad.top, width: track.width }}
     , bottom: { offset: { x: browser.width  - pad.right
                         , y: browser.height - pad.bottom }
               , size:   { height: pad.bottom, width: track.width }}
    }


setBrowserCanvasSize :: Canvas.Dimensions
                     -> BrowserCanvas
                     -> Eff _ BrowserCanvas
setBrowserCanvasSize dim (BrowserCanvas bc) = do

  setContainerStyle bc.container dim
  let trackDim = subtractPadding dim bc.trackPadding

  track <- setTrackCanvasSize trackDim bc.track

  setBufferedCanvasSize dim bc.trackOverlay
  _ <- Canvas.setCanvasDimensions dim bc.staticOverlay

  pure $ BrowserCanvas
       $ bc { dimensions = dim
            , track = track }


setBrowserContainerSize :: Canvas.Dimensions
                        -> BrowserContainer
                        -> Eff _ Unit
setBrowserContainerSize dim bc'@(BrowserContainer bc) = do
  setContainerStyle bc.element dim
  _ <- traverseLayers (resizeLayer dim) bc'
  pure unit



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

  trackOverlay  <- createBufferedCanvas { width: trackDim.width
                                        , height: dimensions.height } "track"

  staticOverlay <- createCanvas dimensions "staticOverlay"

  let trackEl = _.front  $ unwrap
                $ _.canvas $ unwrap $ track

      trackOverlayEl = _.front $ unwrap $ trackOverlay

  setCanvasZIndex trackEl 10
  setCanvasZIndex trackOverlayEl 20
  setCanvasZIndex staticOverlay 30

  -- TODO handle this nicer maybe idk
  let trackElPosition = { left: trackPadding.left - trackInnerPad
                        , top:  trackPadding.top  - trackInnerPad }

  setCanvasPosition trackElPosition trackEl
  setCanvasPosition { left: trackPadding.left, top: 0.0 } trackOverlayEl
  setCanvasPosition { left: 0.0, top: 0.0} staticOverlay

  appendCanvasElem el trackEl
  appendCanvasElem el trackOverlayEl
  appendCanvasElem el staticOverlay

  pure $ BrowserCanvas { dimensions
                       , track, trackPadding
                       , trackOverlay, staticOverlay
                       , container: el}


-- TODO calculate based on glyph bounding boxes
glyphBufferSize :: Canvas.Dimensions
glyphBufferSize = { width: 15.0, height: 300.0 }

-- | Creates an *empty* BrowserContainer, to which layers can be added
browserContainer :: Canvas.Dimensions
                 -> BrowserPadding
                 -> Element
                 -> Eff _ BrowserContainer
browserContainer size padding element = do

  setContainerStyle element size

  glyphBuffer <- createCanvas glyphBufferSize "glyphBuffer"

  dimensions <- Ref.newRef { size, padding }
  layers     <- Ref.newRef mempty

  pure $
    BrowserContainer
      { layers, dimensions, element, glyphBuffer }


-- | Set the CSS z-indices of the Layers in the browser, so their
-- | canvases are drawn in the correct order (i.e. as the
-- | BrowserContainer layer list is ordered)
zIndexLayers :: ∀ m.
                MonadEff _ m
             => Warn "Test this!"
             => BrowserContainer
             -> List String
             -> m Unit
zIndexLayers (BrowserContainer bc) order = liftEff do
  layers <- Ref.readRef bc.layers
  let layerNames = Map.keys layers
      n = length layerNames :: Int
  -- if order does not have the same layer names as the BrowserContainer, fail
  if List.null (List.difference order layerNames)
    then do
      void $ forWithIndex order \i ln ->
        traverse_ (\l -> setCanvasZIndex (l ^. _FrontCanvas) i)
          $ Map.lookup ln layers

    else
      unsafeCrashWith "Called `zIndexLayers` with an order that did not contain all layers"


traverseLayers :: ∀ m a.
                  MonadEff _ m
               => (LayerCanvas -> m a)
               -> BrowserContainer
               -> m (Map String a)
traverseLayers f bc = do
  layers <- getLayers bc
  traverse f layers


getLayers :: ∀ m.
             MonadEff _ m
          => BrowserContainer
          -> m (Map String LayerCanvas)
getLayers (BrowserContainer bc) =
  liftEff $ Ref.readRef bc.layers


resizeLayer :: ∀ m.
               MonadEff _ m
            => Canvas.Dimensions
            -> LayerCanvas
            -> m Unit
resizeLayer dims lc = liftEff do
  case lc of
    Static e -> void $ Canvas.setCanvasDimensions dims e
    Buffer b -> setBufferedCanvasSize dims b



-- addLayers :: BrowserContainer
--           -> List LayerContainer
--           -> Eff _ Unit
-- addLayers bc = traverse_ (appendLayer bc)

-- deleteLayers :: BrowserContainer
--              -> Eff _ Unit
-- deleteLayers (BrowserContainer bc) = do
--   Ref.writeRef bc.layers mempty
  -- TODO update the DOM appropriately,
  -- by just removing the children of bc.element??? or be more specific

renderGlyphs :: TrackCanvas
             -> { drawing :: Drawing, points :: Array Point }
             -> Eff _ Unit
renderGlyphs (TrackCanvas tc) {drawing, points} = do
  glyphBfr <- Canvas.getContext2D tc.glyphBuffer
  ctx <- getBufferedContext tc.canvas

  let w = glyphBufferSize.width
      h = glyphBufferSize.height
      x0 = w / 2.0
      y0 = h / 2.0

  clearCanvas glyphBfr glyphBufferSize

  Drawing.render glyphBfr
    $ Drawing.translate x0 y0 drawing

  runEffFn4 drawCopies tc.glyphBuffer glyphBufferSize ctx points


renderGlyphs' :: CanvasElement
              -- -> LayerContainer
              -> Canvas.Context2D
              -> { drawing :: Drawing, points :: Array Point }
              -> Eff _ Unit
renderGlyphs' glyphBuffer ctx {drawing, points} = do
  glyphCtx <- Canvas.getContext2D glyphBuffer

  let w = glyphBufferSize.width
      h = glyphBufferSize.height
      x0 = w / 2.0
      y0 = h / 2.0

  clearCanvas glyphCtx glyphBufferSize

  Drawing.render glyphCtx
    $ Drawing.translate x0 y0 drawing

  runEffFn4 drawCopies glyphBuffer glyphBufferSize ctx points


type Label = { text :: String, point :: Point, gravity :: LabelPlace }

data LabelPlace = LLeft | LCenter | LRight

derive instance eqLabelPlace :: Eq LabelPlace


-- hardcoded global label font for now
-- can't use Drawing.Font because we need raw Canvas to measure the text size,
-- but we can at least use Drawing to generate the font string

labelFontSize :: Int
labelFontSize = 14

labelFont :: String
labelFont = Drawing.fontString $ Drawing.font sansSerif labelFontSize mempty

-- | Calculate the rectangle covered by a label when it'd be rendered
-- | to a provided canvas context
labelBox :: Context2D -> Label -> Eff _ Canvas.Rectangle
labelBox ctx {text, point, gravity} = do
  {width} <- Canvas.withContext ctx do
    _ <- Canvas.setFont labelFont ctx
    Canvas.measureText ctx text

  -- close enough height, actually calculating it is a nightmare
  let height = Int.toNumber labelFontSize
      pad = 14.0 + width / 2.0
      x = case gravity of
        LCenter -> point.x
        LLeft   -> point.x - pad
        LRight  -> point.x + pad

  pure $ { x
         , y: point.y - height
         , w: width, h: height }


-- | Returns `true` if the input rectangles overlap
isOverlapping :: Canvas.Rectangle
              -> Canvas.Rectangle
              -> Boolean
isOverlapping r1 r2 =
     r1.x < r2.x + r2.w
  && r1.x + r1.w > r2.x
  && r1.y < r2.y + r2.h
  && r1.h + r1.y > r2.y

eqRectangle :: Canvas.Rectangle
            -> Canvas.Rectangle
            -> Boolean
eqRectangle {x,y,w,h} r =
     x == r.x && y == r.y
  && w == r.w && h == r.h


appendBoxed :: ∀ r.
               Array {rect :: Canvas.Rectangle | r}
            -> {rect :: Canvas.Rectangle | r}
            -> Array {rect :: Canvas.Rectangle | r}
appendBoxed sofar next =
  let overlapsAny :: Boolean
      overlapsAny = any overlaps sofar
        where overlaps r' = (not $ eqRectangle next.rect r'.rect)
                            && next.rect `isOverlapping` r'.rect
  in if overlapsAny then sofar else sofar <> [next]


renderLabels :: Array Label -> Context2D -> Eff _ Unit
renderLabels ls ctx = do

  boxed <- traverse (\l -> {text: l.text, rect: _} <$> labelBox ctx l) ls

  let toRender = foldl appendBoxed [] boxed

  Canvas.withContext ctx do
    _ <- Canvas.setFont labelFont ctx

    for_ toRender \box ->
      Canvas.fillText ctx box.text
        (box.rect.x - (box.rect.w / 2.0))
        box.rect.y

type Renderable r = { drawings :: Array { drawing :: Drawing
                                        , points :: Array Point }
                    , labels :: Array Label | r }

renderBrowser :: ∀ a b c.
                 Milliseconds
              -> BrowserCanvas
              -> Number
              -> { tracks     :: { snps :: Renderable a
                                 , annotations :: Renderable b }
                 , relativeUI :: Drawing
                 , fixedUI :: Drawing }
             -> Aff _ Unit
renderBrowser d (BrowserCanvas bc) offset ui = do

  let
      labels = ui.tracks.snps.labels <> ui.tracks.annotations.labels

  -- Render the UI
  liftEff do
    staticOverlayCtx <- Canvas.getContext2D bc.staticOverlay

    clearCanvas staticOverlayCtx bc.dimensions
    Drawing.render staticOverlayCtx ui.fixedUI

    translateBuffer {x: zero, y: zero} bc.trackOverlay
    blankBuffer bc.trackOverlay

    translateBuffer {x: (-offset), y: zero} bc.trackOverlay
    trackOverlayCtx <- getBufferedContext bc.trackOverlay

    Drawing.render trackOverlayCtx ui.relativeUI

    translateBuffer {x: (-offset), y: bc.trackPadding.top} bc.trackOverlay
    renderLabels labels trackOverlayCtx


  -- Render the tracks
  let bfr = (unwrap bc.track).canvas
      cnv = (unwrap bfr).front

  -- TODO extract functions `clearTrack` and `translateTrack` that bake in padding
  ctx <- liftEff $ Canvas.getContext2D cnv
  liftEff do
    dim <- Canvas.getCanvasDimensions cnv

    translateBuffer {x: zero, y: zero} bfr
    clearCanvas ctx $ trackTotalDimensions dim

  liftEff $ translateBuffer { x: trackInnerPad - offset
                            , y: trackInnerPad } bfr

  let snpsTrack  = ui.tracks.snps.drawings
      annotTrack = ui.tracks.annotations.drawings

  for_ [snpsTrack, annotTrack] \t ->
    for_ t \s -> do
      liftEff $ renderGlyphs bc.track s
      delay d


data LayerCanvas =
    Static Canvas.CanvasElement
  | Buffer BufferedCanvas


_FrontCanvas :: Getter' LayerCanvas CanvasElement
_FrontCanvas = to case _ of
  Static c -> c
  Buffer c -> (unwrap c).front

_Buffer :: Prism' LayerCanvas BufferedCanvas
_Buffer = prism' Buffer from
  where from (Static _) = Nothing
        from (Buffer b) = Just b

_Static :: Prism' LayerCanvas CanvasElement
_Static = prism' Static $ case _ of
  Static el -> Just el
  Buffer _  -> Nothing

type LayerRenderable =
  Variant ( fixed    :: Drawing
          , drawings :: Array { drawing :: Drawing
                              , points :: Array Point }
          , labels   :: Array Label )

_fixed :: SProxy "fixed"
_fixed = SProxy

_drawings :: SProxy "drawings"
_drawings = SProxy

_labels :: SProxy "labels"
_labels = SProxy

-- | A renderable `Layer` contains all the "DOM agnostic" parts required to define *any* layer;
-- | by providing a BrowserContainer (which has a BrowserDimensions), a "physical"
-- | canvas with all the required bits can be created, which can then be rendered
-- | by providing a configuration!


-- | Provided a BrowserContainer, we can initialize and add a named layer.
-- | This returns a function that can be used to render to the layer maybe idk????
-- | If the layer already existed, overwrites it

createAndAddLayer :: ∀ m c.
                     MonadEff _ m
                  => BrowserContainer
                  -> String
                  -> Layer (c -> Canvas.Dimensions -> List LayerRenderable)
                  -> m (Tuple String (Number -> c -> m Unit))
createAndAddLayer bc name layer@(Layer lt _ com) = do

  dims <- getDimensions bc
  -- 1. create the layercanvas
  let slots = browserSlots dims

      {size, pos} = { size: slots.full.size,   pos: { left: 0.0, top: 0.0 } }

  canvas <- liftEff do
    cv <- case lt of
      Fixed     -> Static <$> createCanvas         size name
      Scrolling -> Buffer <$> createBufferedCanvas size name
    setCanvasPosition pos (cv ^. _FrontCanvas)
    pure cv

  -- 2. add the canvas to the browsercontainer & DOM
  let layerRef :: _
      layerRef = _.layers $ unwrap bc
  liftEff do
    Ref.modifyRef layerRef $ Map.insert name canvas
    appendCanvasElem (unwrap bc).element $ canvas ^. _FrontCanvas

  -- 3. create the rendering function
  let
      render :: Number -> c -> m Unit
      render offset c = do
        dims <- getDimensions bc

        let
            slots' = browserSlots dims

            toRender :: List LayerRenderable
            toRender = case com of
              Full _ -> (com ^. _Component) c slots'.full.size
              Padded _ _ -> (com ^. _Component) c slots'.padded.size
              _ -> (com ^. _Component) c slots'.full.size -- TODO fix the ui slots

        layers <- liftEff $ Ref.readRef layerRef

        -- TODO handle exceptions!!! :|
        el  <- case Map.lookup name layers of
                 Nothing -> unsafeCrashWith $ "Tried to render layer '" <> name <> "', but it did not exist!"
                 Just e  -> pure $ e ^. _FrontCanvas
        ctx <- slotContext com dims el
        liftEff $ Canvas.withContext ctx do
          setContextTranslation {x: zero, y: zero} ctx
          void $ Canvas.clearRect ctx { x: 0.0, y: 0.0
                                      , w: slots.full.size.width
                                      , h: slots.full.size.height }

        -- temporary hack to offset scrolling tracks as needed
        _ <- liftEff $ Canvas.translate { translateX: -offset, translateY: 0.0 } ctx


        -- use the List LayerRenderable to draw to the canvas
        let
            fixed :: Drawing -> m _
            fixed d = liftEff do
              log "drawing UI!"
              Drawing.render ctx d

            drawings :: Array { drawing :: Drawing, points :: Array Point }
                     -> m _
            drawings ds = liftEff do
              log $ "drawing glyphs: " <> show (Array.length ds)
              for_ ds $ renderGlyphs' (_.glyphBuffer $ unwrap bc) ctx

            labels :: Array _ -> m _
            labels ls = liftEff do
              log "drawing labels"
              renderLabels ls ctx

        for_ toRender
          $ case_ # onMatch { fixed, drawings, labels }

  pure $ Tuple name render
