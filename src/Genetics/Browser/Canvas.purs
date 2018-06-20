-- | This module provides a HTML5 canvas interface for the genetics browser,
-- | which wraps and optimizes all rendering calls

module Genetics.Browser.Canvas where


import Prelude

import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (any, foldl, for_, length)
import Data.Int as Int
import Data.Lens (Getter', Lens', Prism', iso, preview, prism', to, view, (^.))
import Data.Lens.Iso (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record as Lens
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Variant (Variant, case_, onMatch)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
import Genetics.Browser.Layer (BrowserDimensions, BrowserPadding, Component(CBottom, CRight, CLeft, CTop, Padded, Full), Layer(Layer), LayerType(Scrolling, Fixed), _Component, asSlot, browserSlots, setContextTranslation, slotContext, slotRelative)
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, Point)
import Graphics.Drawing (render, translate) as Drawing
import Graphics.Drawing.Font (font, fontString) as Drawing
import Graphics.Drawing.Font (sansSerif)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Node as DOM


_Element :: Iso' CanvasElement Element
_Element = iso unsafeCoerce unsafeCoerce

-- | Create a new CanvasElement, not attached to the DOM, with the provided String as its CSS class
foreign import createCanvas :: { width :: Number, height :: Number }
                            -> String
                            -> Effect CanvasElement


foreign import setElementStyleImpl :: EffectFn3
                                      Element String String
                                      Unit

setElementStyle :: Element
                -> String
                -> String
                -> Effect Unit
setElementStyle = runEffectFn3 setElementStyleImpl

setElementStyles :: Element -> Array (Tuple String String) -> Effect Unit
setElementStyles el =
  traverse_ (uncurry $ setElementStyle el)

setCanvasStyles :: CanvasElement -> Array (Tuple String String) -> Effect Unit
setCanvasStyles = setElementStyles <<< view _Element


setCanvasStyle :: CanvasElement
               -> String
               -> String
               -> Effect Unit
setCanvasStyle ce = setElementStyle (ce ^. _Element)

setCanvasZIndex :: CanvasElement -> Int -> Effect Unit
setCanvasZIndex ce i = setCanvasStyle ce "z-index" (show i)

setCanvasPosition :: ∀ r.
                     { left :: Number, top :: Number | r }
                  -> CanvasElement
                  -> Effect Unit
setCanvasPosition {left, top} ce =
  setCanvasStyles ce
    [ Tuple "position" "absolute"
    , Tuple "top"  (show top  <> "px")
    , Tuple "left" (show left <> "px") ]


foreign import appendCanvasElem :: Element
                                -> CanvasElement
                                -> Effect Unit


-- | Sets some of the browser container's CSS to reasonable defaults
foreign import setContainerStyle :: Element -> Canvas.Dimensions -> Effect Unit


foreign import drawCopies :: EffectFn4
                             CanvasElement Canvas.Dimensions Context2D (Array Point)
                             Unit


foreign import setCanvasTranslation :: Point
                                    -> CanvasElement
                                    -> Effect Unit

foreign import elementClickImpl :: EffectFn2
                                   Element (Point -> Effect Unit)
                                   Unit

elementClick :: Element
             -> (Point -> Effect Unit)
             -> Effect Unit
elementClick e = runEffectFn2 elementClickImpl e


-- | Attaches a callback to the browsercontainer, to be called when
-- | the browser element is clicked. The callback is called with
-- | coordinates relative to the provided component slot.
browserClickHandler :: ∀ m.
                       MonadEffect m
                    => BrowserContainer
                    -> Component (Point -> Effect Unit)
                    -> m Unit
browserClickHandler bc com = liftEffect do
  dims <- getDimensions bc

  let cb = com ^. _Component
      translate = slotRelative dims (asSlot com)

  elementClick (bc ^. _Container) (cb <<< translate)



foreign import scrollCanvasImpl :: EffectFn3
                                   CanvasElement CanvasElement Point
                                   Unit

-- | Scroll a buffered canvas by copying its contents to its backbuffer,
-- | then copying it back at an offset
scrollCanvas :: BufferedCanvas
             -> Point
             -> Effect Unit
scrollCanvas (BufferedCanvas bc) = runEffectFn3 scrollCanvasImpl bc.back bc.front

-- | Scroll all buffered canvases in a container
scrollBrowser :: BrowserContainer
              -> Point
              -> Effect Unit
scrollBrowser (BrowserContainer bc) pt = do
  layers <- Ref.read bc.layers
  -- | Filter out all layers that don't scroll
  let scrolling = filterMap (preview _Buffer) layers
  -- | then... scroll them
  for_ scrolling \bc' -> scrollCanvas bc' pt



foreign import canvasDragImpl :: CanvasElement
                              -> ( { during :: Nullable Point
                                   , total :: Nullable Point } -> Effect Unit )
                              -> Effect Unit


foreign import canvasWheelCBImpl :: CanvasElement
                                 -> (Number -> Effect Unit)
                                 -> Effect Unit

-- |
canvasDrag :: CanvasElement
           -> (Either Point Point -> Effect Unit)
           -> Effect Unit
canvasDrag el f =
  let toEither g {during, total} = case toMaybe during of
        Just p  -> g $ Right p
        Nothing -> g $ Left $ fromMaybe {x:zero, y:zero} $ toMaybe total
  in canvasDragImpl el (toEither f)


-- | Takes a BrowserContainer and a callback function that is called with the
-- | total dragged distance when a click & drag action is completed.
dragScroll :: BrowserContainer
           -> (Point -> Effect Unit)
           -> Effect Unit
dragScroll bc'@(BrowserContainer {element}) cb =
  canvasDrag (unsafeCoerce element) f -- actually safe
  where f = case _ of
              Left  p   -> cb p
              Right {x} -> do
                let p' = {x: -x, y: 0.0}
                scrollBrowser bc' p'


-- | Takes a BrowserContainer and a callback function that is called with each
-- | wheel scroll `deltaY`. Callback is provided with only the sign of `deltaY`
-- | as to be `deltaMode` agnostic.
wheelZoom :: BrowserContainer
          -> (Number -> Effect Unit)
          -> Effect Unit
wheelZoom bc cb =
  canvasWheelCBImpl
    (unsafeCoerce $ _.element $ unwrap bc) cb


-- | Helper function for erasing the contents of a canvas context given its dimensions
clearCanvas :: Context2D -> Canvas.Dimensions -> Effect Unit
clearCanvas ctx {width, height} =
  void $ Canvas.clearRect ctx { x: 0.0, y: 0.0, w: width, h: height }




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
                     -> Effect BufferedCanvas
createBufferedCanvas dim name = do
  back  <- createCanvas dim $ name <> "-buffer"
  front <- createCanvas dim $ name
  let bc = BufferedCanvas { back, front }
  blankBuffer bc
  pure bc

getBufferedContext :: BufferedCanvas
                   -> Effect Context2D
getBufferedContext = Canvas.getContext2D <$> _.front <<< unwrap

setBufferedCanvasSize :: Canvas.Dimensions
                      -> BufferedCanvas
                      -> Effect Unit
setBufferedCanvasSize dim bc@(BufferedCanvas {back, front}) = do
  _ <- Canvas.setCanvasDimensions back dim
  _ <- Canvas.setCanvasDimensions front dim
  blankBuffer bc
  pure unit


translateBuffer :: Point
                -> BufferedCanvas
                -> Effect Unit
translateBuffer p (BufferedCanvas bc) = do
  setCanvasTranslation p bc.back
  setCanvasTranslation p bc.front

blankBuffer :: BufferedCanvas
            -> Effect Unit
blankBuffer bc@(BufferedCanvas {back, front}) = do
  translateBuffer {x: zero, y: zero} bc

  dim <- Canvas.getCanvasDimensions back

  for_ [back, front]
    $ flip clearCanvas dim <=< Canvas.getContext2D




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

_Dimensions :: ∀ n r.
               Newtype n { dimensions :: Canvas.Dimensions | r }
            => Lens' n Canvas.Dimensions
_Dimensions = _Newtype <<< Lens.prop (SProxy :: SProxy "dimensions")

trackTotalDimensions :: Canvas.Dimensions -> Canvas.Dimensions
trackTotalDimensions d = { width:  d.width  + extra
                         , height: d.height + extra }
  where extra = 2.0 * trackInnerPad


trackCanvas :: Canvas.Dimensions
            -> Effect TrackCanvas
trackCanvas dim = do
  canvas <- createBufferedCanvas (trackTotalDimensions dim) "track"
  glyphBuffer <- createCanvas glyphBufferSize "glyphBuffer"

  pure $ TrackCanvas { dimensions: dim
                     , canvas, glyphBuffer }

setTrackCanvasSize :: Canvas.Dimensions
                   -> TrackCanvas
                   -> Effect TrackCanvas
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
                 MonadEffect m
              => BrowserContainer
              -> m BrowserDimensions
getDimensions (BrowserContainer {dimensions}) =
  liftEffect $ Ref.read dimensions

_Layers :: Lens' BrowserContainer (Ref (Map String LayerCanvas))
_Layers = _Newtype <<< Lens.prop (SProxy :: SProxy "layers")

_Container :: Lens' BrowserContainer Element
_Container = _Newtype <<< Lens.prop (SProxy :: SProxy "element")


_Track :: Lens' BrowserCanvas TrackCanvas
_Track = _Newtype <<< Lens.prop (SProxy :: SProxy "track")


foreign import debugBrowserCanvas :: String
                                  -> BrowserCanvas
                                  -> Effect Unit


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


uiSlots :: BrowserCanvas
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
                     -> Effect BrowserCanvas
setBrowserCanvasSize dim (BrowserCanvas bc) = do

  setContainerStyle bc.container dim
  let trackDim = subtractPadding dim bc.trackPadding

  track <- setTrackCanvasSize trackDim bc.track

  setBufferedCanvasSize dim bc.trackOverlay
  _ <- Canvas.setCanvasDimensions bc.staticOverlay dim

  pure $ BrowserCanvas
       $ bc { dimensions = dim
            , track = track }


setBrowserContainerSize :: ∀ m.
                           MonadEffect m
                        => Canvas.Dimensions
                        -> BrowserContainer
                        -> m Unit
setBrowserContainerSize dim bc'@(BrowserContainer bc) = liftEffect $ do
  Ref.modify_ (_ { size = dim }) bc.dimensions
  setContainerStyle bc.element dim
  traverse_ (resizeLayer dim) =<< getLayers bc'


-- | Creates a `BrowserCanvas` and appends it to the provided element.
-- | Resizes the container element to fit.
browserCanvas :: Canvas.Dimensions
              -> TrackPadding
              -> Element
              -> Effect BrowserCanvas
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
                 -> Effect BrowserContainer
browserContainer size padding element = do

  setContainerStyle element size

  glyphBuffer <- createCanvas glyphBufferSize "glyphBuffer"

  dimensions <- Ref.new { size, padding }
  layers     <- Ref.new mempty

  pure $
    BrowserContainer
      { layers, dimensions, element, glyphBuffer }


-- | Set the CSS z-indices of the Layers in the browser, so their
-- | canvases are drawn in the correct order (i.e. as the
-- | BrowserContainer layer list is ordered)
zIndexLayers :: ∀ m.
                MonadEffect m
             => BrowserContainer
             -> List String
             -> m Unit
zIndexLayers (BrowserContainer bc) order = liftEffect do
  layers <- Ref.read bc.layers
  let layerNames = Map.keys layers
      n = length layerNames :: Int
  -- if order does not have the same layer names as the BrowserContainer, fail
  if List.null (List.difference order $ List.fromFoldable layerNames)
    then do
      void $ forWithIndex order \i ln ->
        traverse_ (\l -> setCanvasZIndex (l ^. _FrontCanvas) i)
          $ Map.lookup ln layers

    else
      unsafeCrashWith "Called `zIndexLayers` with an order that did not contain all layers"


getLayers :: ∀ m.
             MonadEffect m
          => BrowserContainer
          -> m (Map String LayerCanvas)
getLayers (BrowserContainer bc) =
  liftEffect $ Ref.read bc.layers


resizeLayer :: ∀ m.
               MonadEffect m
            => Canvas.Dimensions
            -> LayerCanvas
            -> m Unit
resizeLayer dims lc = liftEffect do
  case lc of
    Static e -> void $ Canvas.setCanvasDimensions e dims
    Buffer b -> setBufferedCanvasSize dims b

-- | Add a LayerCanvas with the provided name to the browser container
-- | and its element. The LayerCanvas' front element is added without
-- | a Z-index, and replaces any existing layer with the same name!
addLayer :: ∀ m.
            MonadEffect m
         => BrowserContainer
         -> String
         -> LayerCanvas
         -> m Unit
addLayer bc name lc = liftEffect do
  layers <- getLayers bc

  Ref.modify_  (Map.insert name lc) (bc ^. _Layers)
  let toNode l = unsafeCoerce $ l ^. _FrontCanvas

  void $ case Map.lookup name layers of
    Just oldL  -> DOM.replaceChild (toNode lc) (toNode oldL) (unsafeCoerce $ bc ^. _Container)
    Nothing    -> DOM.appendChild  (toNode lc)               (unsafeCoerce $ bc ^. _Container)

-- | Delete a LayerCanvas by name from the browser container, and the
-- | container DOM element.
deleteLayer :: ∀ m.
               MonadEffect m
            => BrowserContainer
            -> String
            -> m Unit
deleteLayer bc name = liftEffect do
  layers <- getLayers bc
  let toNode l = unsafeCoerce $ l ^. _FrontCanvas

  case Map.lookup name layers of
    Nothing -> pure unit
    Just l  -> void $ DOM.removeChild (toNode l) (unsafeCoerce $ bc ^. _Container)

  Ref.modify_ (Map.delete name) $ (bc ^. _Layers)


renderGlyphs :: TrackCanvas
             -> { drawing :: Drawing, points :: Array Point }
             -> Effect Unit
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

  runEffectFn4 drawCopies tc.glyphBuffer glyphBufferSize ctx points


renderGlyphs' :: CanvasElement
              -- -> LayerContainer
              -> Canvas.Context2D
              -> { drawing :: Drawing, points :: Array Point }
              -> Effect Unit
renderGlyphs' glyphBuffer ctx {drawing, points} = do
  glyphCtx <- Canvas.getContext2D glyphBuffer

  let w = glyphBufferSize.width
      h = glyphBufferSize.height
      x0 = w / 2.0
      y0 = h / 2.0

  clearCanvas glyphCtx glyphBufferSize

  Drawing.render glyphCtx
    $ Drawing.translate x0 y0 drawing

  runEffectFn4 drawCopies glyphBuffer glyphBufferSize ctx points


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
labelBox :: Context2D -> Label -> Effect Canvas.Rectangle
labelBox ctx {text, point, gravity} = do
  {width} <- Canvas.withContext ctx do
    _ <- Canvas.setFont ctx labelFont
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


renderLabels :: Array Label -> Context2D -> Effect Unit
renderLabels ls ctx = do

  boxed <- traverse (\l -> {text: l.text, rect: _} <$> labelBox ctx l) ls

  let toRender = foldl appendBoxed [] boxed

  Canvas.withContext ctx do
    _ <- Canvas.setFont ctx labelFont

    for_ toRender \box ->
      Canvas.fillText ctx box.text
        (box.rect.x - (box.rect.w / 2.0))
        box.rect.y

type Renderable' r = { drawings :: Array { drawing :: Drawing
                                        , points :: Array Point }
                    , labels :: Array Label | r }



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

type Renderable =
  Variant ( static   :: Drawing
          , drawings :: Array { drawing :: Drawing
                              , points :: Array Point }
          , labels   :: Array Label )

_static   = SProxy :: SProxy "static"
_drawings = SProxy :: SProxy "drawings"
_labels   = SProxy :: SProxy "labels"

type RenderableLayer c = Layer (c -> Canvas.Dimensions -> List Renderable)

type RenderableLayerHotspots c a =
  Layer (c
         -> Canvas.Dimensions
         -> { renderables :: List Renderable
            , hotspots :: Number -> Point -> Array a })

-- | A renderable `Layer` contains all the "DOM agnostic" parts required to define *any* layer;
-- | by providing a BrowserContainer (which has a BrowserDimensions), a "physical"
-- | canvas with all the required bits can be created, which can then be rendered
-- | by providing a configuration!




-- | Provided a BrowserContainer, we can initialize and add a named layer.
-- | This returns a function that can be used to render to the layer maybe idk????
-- | If the layer already existed, overwrites it
createAndAddLayer :: ∀ m c a.
                     MonadEffect m
                  => BrowserContainer
                  -> String
                  -> RenderableLayerHotspots c a
                  -> m { render :: c -> m (List Renderable)
                       , drawOnCanvas :: Number -> List Renderable -> m Unit
                       , lastHotspots :: m (Number -> Point -> Array a) }
createAndAddLayer bc name layer@(Layer lt _ com) = do

  dims <- getDimensions bc
  -- 1. create the layercanvas
  let slots = browserSlots dims

      {size, pos} = { size: slots.full.size,   pos: { left: 0.0, top: 0.0 } }

  canvas <- liftEffect do
    cv <- case lt of
      Fixed     -> Static <$> createCanvas         size name
      Scrolling -> Buffer <$> createBufferedCanvas size name
    setCanvasPosition pos (cv ^. _FrontCanvas)
    pure cv


  hotspotsRef <- liftEffect $ Ref.new { offset: 0.0, hotspots: (\r p -> []) }
  let lastHotspots = liftEffect do
        { offset, hotspots } <- Ref.read hotspotsRef
        pure \r {x, y} -> hotspots r {x: x+offset, y}

  -- 2. add the canvas to the browsercontainer & DOM
  let layerRef = _.layers $ unwrap bc
  liftEffect do
    Ref.modify_ (Map.insert name canvas) layerRef
    appendCanvasElem (unwrap bc).element $ canvas ^. _FrontCanvas

  -- 3. create the rendering function
  let render :: c -> m (List Renderable)
      render c = do
        dims' <- getDimensions bc

        let slots' = browserSlots dims'

            toRender = (com ^. _Component) c $ case com of
              Full     _ -> slots'.full.size
              Padded _ _ -> slots'.padded.size
              CTop     _ -> slots'.top.size
              CLeft    _ -> slots'.left.size
              CRight   _ -> slots'.right.size
              CBottom  _ -> slots'.bottom.size


        liftEffect $ Ref.modify_ (_ { hotspots = toRender.hotspots }) hotspotsRef

        pure toRender.renderables


      drawOnCanvas :: Number -> List Renderable -> m Unit
      drawOnCanvas offset renderables = do
        layers <- getLayers bc

        liftEffect $ Ref.modify_ ( _ { offset = offset }) hotspotsRef
        -- TODO handle exceptions!!! :|
        el  <- case Map.lookup name layers of
                 Nothing -> unsafeCrashWith $ "Tried to render layer '" <> name <> "', but it did not exist!"
                 Just e  -> pure $ e ^. _FrontCanvas

        dims' <- getDimensions bc
        ctx <- slotContext layer dims' el

        liftEffect $ Canvas.withContext ctx do
          setContextTranslation {x: zero, y: zero} ctx
          void $ Canvas.clearRect ctx { x: 0.0, y: 0.0
                                      , w: slots.full.size.width
                                      , h: slots.full.size.height }

        -- temporary hack to offset scrolling tracks as needed

        _ <- liftEffect $ Canvas.translate ctx { translateX: -offset, translateY: 0.0 }
        -- use the List Renderable to draw to the canvas
        let
            static :: Drawing -> m Unit
            static d = liftEffect $ Drawing.render ctx d

            drawings :: Array { drawing :: Drawing, points :: Array Point }
                     -> m Unit
            drawings ds = liftEffect $ for_ ds
                            $ renderGlyphs' (_.glyphBuffer $ unwrap bc) ctx

            labels :: Array Label -> m Unit
            labels ls = liftEffect $ renderLabels ls ctx

        for_ renderables
          $ case_ # onMatch { static, drawings, labels }

  pure { render, drawOnCanvas, lastHotspots }



-- | Used for layers that don't contain clickable features
createAndAddLayer_ :: ∀ m c.
                     MonadEffect m
                  => BrowserContainer
                  -> String
                  -> RenderableLayer c
                  -> m { render :: c -> m (List Renderable)
                       , drawOnCanvas :: Number -> List Renderable -> m Unit }
createAndAddLayer_ bc name layer@(Layer lt _ com) = do
  -- silly solution to avoid code duping createAndAddLayer
  let layer' :: ∀ a. RenderableLayerHotspots c a
      layer' = (map <<< map <<< map) (\ renderables -> { renderables, hotspots: \r p -> [] }) layer

  { render, drawOnCanvas } <- createAndAddLayer bc name layer'
  pure { render, drawOnCanvas }
