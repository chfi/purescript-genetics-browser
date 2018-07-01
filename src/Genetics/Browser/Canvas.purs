-- | This module provides a HTML5 canvas interface for the genetics browser,
-- | which wraps and optimizes all rendering calls

module Genetics.Browser.Canvas
       ( TrackContainer
       , Renderable
       , BatchDrawing
       , ContentLayer
       , RenderableLayer
       , RenderableLayerHotspots
       , Label
       , LabelPlace(..)
       , _drawing
       , _drawingBatch
       , _labels
       , createAndAddLayer
       , createAndAddLayer_
       , renderLayer
       , getDimensions
       , _Container
       , dragScroll
       , wheelZoom
       , trackClickHandler
       , trackContainer
       , setTrackContainerSize
       , setElementStyle
       ) where


import Prelude

import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (any, foldl, for_, length)
import Data.Int as Int
import Data.Lens (Getter', Lens', Prism', preview, prism', to, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record as Lens
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.Pair (Pair(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Variant (Variant, case_, onMatch)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn5, runEffectFn2, runEffectFn3, runEffectFn5)
import Genetics.Browser.Layer (Component, Layer(Layer), LayerType(Scrolling, Fixed), TrackDimensions, TrackPadding, _Component, asSlot, setContextTranslation, slotContext, slotOffset, slotSize)
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, Point)
import Graphics.Drawing (render, translate) as Drawing
import Graphics.Drawing.Font (font, fontString) as Drawing
import Graphics.Drawing.Font (sansSerif)
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Node as DOM



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
setCanvasStyles = setElementStyles <<< unsafeCoerce


setCanvasStyle :: CanvasElement
               -> String
               -> String
               -> Effect Unit
setCanvasStyle = setElementStyle <<< unsafeCoerce

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


-- | Sets some of the track container's CSS to reasonable defaults
foreign import setContainerStyle :: Element -> Canvas.Dimensions -> Effect Unit


foreign import drawCopies
  :: EffectFn5
     CanvasElement
     Canvas.Dimensions
     Context2D
     (Point -> Boolean)
     (Array Point)
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


-- | Attaches a callback to the trackcontainer, to be called when
-- | the track element is clicked. The callback is called with
-- | coordinates relative to the provided component slot.
trackClickHandler :: ∀ m.
                       MonadEffect m
                    => TrackContainer
                    -> Component (Point -> Effect Unit)
                    -> m Unit
trackClickHandler bc com = liftEffect do
  dims <- getDimensions bc

  let cb = com ^. _Component
      translate p = p - (slotOffset dims $ asSlot com)

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
scrollTrack :: TrackContainer
            -> Point
            -> Effect Unit
scrollTrack (TrackContainer bc) pt = do
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


-- | Takes a TrackContainer and a callback function that is called with the
-- | total dragged distance when a click & drag action is completed.
dragScroll :: TrackContainer
           -> (Point -> Effect Unit)
           -> Effect Unit
dragScroll bc'@(TrackContainer {element}) cb =
  canvasDrag (unsafeCoerce element) case _ of
    Left  p   -> cb p
    Right {x} -> do
      let p' = {x: -x, y: 0.0}
      scrollTrack bc' p'


-- | Takes a TrackContainer and a callback function that is called with each
-- | wheel scroll `deltaY`. Callback is provided with only the sign of `deltaY`
-- | as to be `deltaMode` agnostic.
wheelZoom :: TrackContainer
          -> (Number -> Effect Unit)
          -> Effect Unit
wheelZoom bc cb =
  canvasWheelCBImpl
    (unsafeCoerce $ _.element $ unwrap bc) cb


-- | Helper function for erasing the contents of a canvas context given its dimensions
clearCanvas :: Context2D -> Canvas.Dimensions -> Effect Unit
clearCanvas ctx {width, height} =
  void $ Canvas.clearRect ctx { x: 0.0, y: 0.0, width, height }


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


newtype TrackContainer =
  TrackContainer { layers      :: Ref (Map String LayerCanvas)
                   , dimensions  :: Ref TrackDimensions
                   , element     :: Element
                   , glyphBuffer :: CanvasElement }


derive instance newtypeTrackContainer :: Newtype TrackContainer _


getDimensions :: ∀ m.
                 MonadEffect m
              => TrackContainer
              -> m TrackDimensions
getDimensions (TrackContainer {dimensions}) =
  liftEffect $ Ref.read dimensions

_Layers :: Lens' TrackContainer (Ref (Map String LayerCanvas))
_Layers = _Newtype <<< Lens.prop (SProxy :: SProxy "layers")

_Container :: Lens' TrackContainer Element
_Container = _Newtype <<< Lens.prop (SProxy :: SProxy "element")


setTrackContainerSize :: ∀ m.
                           MonadEffect m
                        => Canvas.Dimensions
                        -> TrackContainer
                        -> m Unit
setTrackContainerSize dim bc'@(TrackContainer bc) = liftEffect $ do
  Ref.modify_ (_ { size = dim }) bc.dimensions
  setContainerStyle bc.element dim
  traverse_ (resizeLayer dim) =<< getLayers bc'



-- TODO calculate based on glyph bounding boxes
glyphBufferSize :: Canvas.Dimensions
glyphBufferSize = { width: 15.0, height: 300.0 }

-- | Creates an *empty* TrackContainer, to which layers can be added
trackContainer :: Canvas.Dimensions
                 -> TrackPadding
                 -> Element
                 -> Effect TrackContainer
trackContainer size padding element = do

  setContainerStyle element size

  glyphBuffer <- createCanvas glyphBufferSize "glyphBuffer"

  dimensions <- Ref.new { size, padding }
  layers     <- Ref.new mempty

  pure $
    TrackContainer
      { layers, dimensions, element, glyphBuffer }


-- | Set the CSS z-indices of the Layers in the track, so their
-- | canvases are drawn in the correct order (i.e. as the
-- | TrackContainer layer list is ordered)
zIndexLayers :: ∀ m.
                MonadEffect m
             => TrackContainer
             -> List String
             -> m Unit
zIndexLayers (TrackContainer bc) order = liftEffect do
  layers <- Ref.read bc.layers
  let layerNames = Map.keys layers
      n = length layerNames :: Int
  -- if order does not have the same layer names as the TrackContainer, fail
  if List.null (List.difference order $ List.fromFoldable layerNames)
    then do
      void $ forWithIndex order \i ln ->
        traverse_ (\l -> setCanvasZIndex (l ^. _FrontCanvas) i)
          $ Map.lookup ln layers

    else
      unsafeCrashWith "Called `zIndexLayers` with an order that did not contain all layers"


getLayers :: ∀ m.
             MonadEffect m
          => TrackContainer
          -> m (Map String LayerCanvas)
getLayers (TrackContainer bc) =
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

-- | Add a LayerCanvas with the provided name to the track container
-- | and its element. The LayerCanvas' front element is added without
-- | a Z-index, and replaces any existing layer with the same name!
addLayer :: ∀ m.
            MonadEffect m
         => TrackContainer
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

-- | Delete a LayerCanvas by name from the track container, and the
-- | container DOM element.
deleteLayer :: ∀ m.
               MonadEffect m
            => TrackContainer
            -> String
            -> m Unit
deleteLayer bc name = liftEffect do
  layers <- getLayers bc
  let toNode l = unsafeCoerce $ l ^. _FrontCanvas

  case Map.lookup name layers of
    Nothing -> pure unit
    Just l  -> void $ DOM.removeChild (toNode l) (unsafeCoerce $ bc ^. _Container)

  Ref.modify_ (Map.delete name) $ bc ^. _Layers




renderGlyphs :: CanvasElement
             -> Canvas.Context2D
             -> Pair Number
             -> { drawing :: Drawing, points :: Array Point }
             -> Effect Unit
renderGlyphs glyphBuffer ctx (Pair l r) {drawing, points}  = do
  glyphCtx <- Canvas.getContext2D glyphBuffer

  let w = glyphBufferSize.width
      h = glyphBufferSize.height
      x0 = w / 2.0
      y0 = h / 2.0

  clearCanvas glyphCtx glyphBufferSize

  Drawing.render glyphCtx
    $ Drawing.translate x0 y0 drawing

  let pred {x} = x >= l && x <= r

  runEffectFn5 drawCopies glyphBuffer glyphBufferSize ctx pred points


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
         , width, height }


-- | Returns `true` if the input rectangles overlap
isOverlapping :: Canvas.Rectangle
              -> Canvas.Rectangle
              -> Boolean
isOverlapping r1 r2 =
     r1.x             < r2.x + r2.width
  && r1.x + r1.width  > r2.x
  && r1.y             < r2.y + r2.height
  && r1.height + r1.y > r2.y


appendBoxed :: ∀ r.
               Array {rect :: Canvas.Rectangle | r}
            -> {rect :: Canvas.Rectangle | r}
            -> Array {rect :: Canvas.Rectangle | r}
appendBoxed sofar next =
  let overlapsAny :: Boolean
      overlapsAny = any overlaps sofar
        where overlaps r' = (not $ next.rect == r'.rect)
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
        (box.rect.x - (box.rect.width / 2.0))
        box.rect.y



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

type BatchDrawing = { drawing :: Drawing, points :: Array Point }

type Renderable =
  Variant ( drawing   :: Drawing
          , drawingBatch :: Array BatchDrawing
          , labels   :: Array Label )

_drawing   = SProxy :: SProxy "drawing"
_drawingBatch = SProxy :: SProxy "drawingBatch"
_labels   = SProxy :: SProxy "labels"



-- | A `ContentLayer` represents anything that can be produced in the
-- | context of a Layer with a Canvas, and some sort of additional configuration.
type ContentLayer c a = Layer (c -> Canvas.Dimensions -> a)

type RenderableLayer c = ContentLayer c (List Renderable)
type RenderableLayerHotspots c a =
  ContentLayer c { renderables :: List Renderable
                 , hotspots :: Number -> Point -> Array a }



-- | Provided a TrackContainer, we can initialize and add a named layer.
-- | This returns a function that can be used to render to the layer maybe idk????
-- | If the layer already existed, overwrites it
createAndAddLayer :: ∀ m c a.
                     MonadEffect m
                  => TrackContainer
                  -> String
                  -> RenderableLayerHotspots c a
                  -> m { render :: c -> m (List Renderable)
                       , drawOnCanvas :: Pair Number -> List Renderable -> m Unit
                       , lastHotspots :: m (Number -> Point -> Array a) }
createAndAddLayer bc name layer@(Layer lt _ com) = do

  -- 1. create the layercanvas

  size <- _.size <$> getDimensions bc
  let pos = { left: 0.0, top: 0.0 }

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

  -- 2. add the canvas to the trackcontainer & DOM
  let layerRef = _.layers $ unwrap bc
  liftEffect do
    Ref.modify_ (Map.insert name canvas) layerRef
    appendCanvasElem (unwrap bc).element $ canvas ^. _FrontCanvas

  -- 3. create the rendering function
  let render :: c -> m (List Renderable)
      render c = do
        dims <- getDimensions bc

        let toRender = (com ^. _Component) c $ slotSize dims (asSlot com)

        liftEffect $ Ref.modify_ (_ { hotspots = toRender.hotspots }) hotspotsRef

        pure toRender.renderables


      drawOnCanvas :: Pair Number -> List Renderable -> m Unit
      drawOnCanvas (Pair l r) renderables = do
        layers <- getLayers bc

        liftEffect $ Ref.modify_ ( _ { offset = l }) hotspotsRef
        -- TODO handle exceptions!!! :|
        el  <- case Map.lookup name layers of
                 Nothing -> unsafeCrashWith $ "Tried to render layer '" <> name <> "', but it did not exist!"
                 Just e  -> pure $ e ^. _FrontCanvas

        dims <- getDimensions bc
        ctx <- slotContext layer dims el

        liftEffect $ Canvas.withContext ctx do
          setContextTranslation {x: zero, y: zero} ctx
          void $ Canvas.clearRect ctx $ Record.merge {x: 0.0, y: 0.0} dims.size

        -- temporary hack to offset scrolling tracks as needed

        _ <- liftEffect $ Canvas.translate ctx { translateX: -l, translateY: 0.0 }

        -- use the List Renderable to draw to the canvas
        let
            drawing :: Drawing -> m Unit
            drawing d = liftEffect $ Drawing.render ctx d

            drawingBatch :: Array { drawing :: Drawing, points :: Array Point }
                     -> m Unit
            drawingBatch ds = liftEffect $ for_ ds
                            $ renderGlyphs (_.glyphBuffer $ unwrap bc) ctx (Pair l r)

            labels :: Array Label -> m Unit
            labels ls = liftEffect $ renderLabels ls ctx

        for_ renderables
          $ case_ # onMatch { drawing, drawingBatch, labels }

  pure { render, drawOnCanvas, lastHotspots }



-- | Used for layers that don't contain clickable features
createAndAddLayer_ :: ∀ m c.
                     MonadEffect m
                  => TrackContainer
                  -> String
                  -> RenderableLayer c
                  -> m { render :: c -> m (List Renderable)
                       , drawOnCanvas :: Pair Number -> List Renderable -> m Unit }
createAndAddLayer_ bc name layer@(Layer lt _ com) = do
  -- silly solution to avoid code duping createAndAddLayer
  let layer' :: ∀ a. RenderableLayerHotspots c a
      layer' = (map <<< map <<< map) (\ renderables -> { renderables, hotspots: \r p -> [] }) layer

  { render, drawOnCanvas } <- createAndAddLayer bc name layer'
  pure { render, drawOnCanvas }



-- | Convenience function for rendering layers when manipulating the
-- | Renderable contents isn't desired.
renderLayer :: ∀ m c r.
               MonadEffect m
            => Pair Number
            -> c
            -> { render :: c -> m (List Renderable)
               , drawOnCanvas :: Pair Number -> List Renderable -> m Unit
               | r }
            -> m Unit
renderLayer p c l = l.render c >>= l.drawOnCanvas p
