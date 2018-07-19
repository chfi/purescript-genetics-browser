module Genetics.Browser.UI.View where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left))
import Data.Filterable (filterMap)
import Data.Foldable (foldMap, length, sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Lens ((^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (over, unwrap, wrap)
import Data.Pair (Pair(..))
import Data.Pair as Pair
import Data.Symbol (SProxy(..))
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Variant (Variant, case_, inj)
import Data.Variant as V
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, killFiber, launchAff, launchAff_, throwError)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign, MultipleErrors, renderForeignError)
import Genetics.Browser (HexColor(..), LegendConfig, Peak, VScale, pixelSegments)
import Genetics.Browser.Bed (getGenes)
import Genetics.Browser.Canvas (BrowserContainer, TrackContainer, _Container, addTrack, browserContainer, dragScroll, getDimensions, getTrack, setElementStyle, setTrackContainerSize, trackClickHandler, trackContainer, wheelZoom, withLoadingIndicator)
import Genetics.Browser.Coordinates (CoordSys, CoordSysView(..), _Segments, _TotalSize, coordSys, normalizeView, pairsOverlap, scalePairBy, scaleToScreen, translatePairBy, viewScale)
import Genetics.Browser.Demo (Annotation, AnnotationField, AnnotationsConfig, SNP, SNPConfig, addChrLayers, addGWASLayers, addGeneLayers, annotationsForScale, filterSig, getAnnotations, getSNPs, showAnnotationField)
import Genetics.Browser.Layer (Component(Center), TrackPadding, trackSlots)
import Genetics.Browser.Track (class TrackRecord, makeContainers, makeTrack)
import Genetics.Browser.Types (ChrId(ChrId), _NegLog10, _prec)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Point)
import Math (pow)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Prim.RowList (class RowToList)
import Record (insert)
import Record as Record
import Simple.JSON (read)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Document (createElement, documentElement) as DOM
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (setId)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.DOM.ParentNode (querySelector) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument) as DOM
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent (key) as DOM


foreign import onTimeout
  :: Milliseconds
  -> Effect Unit
  -> Effect { run    :: Effect Unit
            , cancel :: Effect Unit }


data UpdateView =
    ScrollView Number
  | ZoomView Number
  | ModView (Pair BigInt -> Pair BigInt)


instance showUpdateView :: Show UpdateView where
  show (ScrollView x) = "(Scroll by " <> show x <> ")"
  show (ZoomView s) = "(Zoom by " <> show s <> ")"
  show _ = "(ModView)"


    -- TODO idk if this instance makes sense??? whatevs
instance semigroupUpdateView :: Semigroup UpdateView where
  append (ScrollView x1) (ScrollView x2) = ScrollView (x1 + x2)
  append (ZoomView s1)   (ZoomView s2)   = ZoomView   (s1 * s2)
  append _ y = y

instance monoidUpdateView :: Monoid UpdateView where
  mempty = ModView identity


updateViewFold :: UpdateView
               -> CoordSysView
               -> CoordSysView
updateViewFold uv = over CoordSysView case uv of
  ZoomView   x -> (_ `scalePairBy`     x)
  ScrollView x -> (_ `translatePairBy` x)
  ModView f    -> f


_global      = SProxy :: SProxy "global"
_pixels      = SProxy :: SProxy "pixels"
_viewLengths = SProxy :: SProxy "viewLengths"

-- | A `Translation` describes a translation of a CoordSysView.
-- | `global` defines a translation in global coordinates (summed Bps, roughly);
-- | `pixels` a translation in pixels to the right, depending on the browser canvas width;
-- | `viewLengths` one in terms of the size of the current view, 1.0 == move one view length to the right.

type Translation =
  Variant ( global      :: BigInt
          , pixels      :: Number
          , viewLengths :: Number )


type Around = { around :: Number, viewLengths :: Number }
type ViewRange = Pair Number

_around = SProxy :: SProxy "around"
_viewRange = SProxy :: SProxy "viewRange"

-- | A `Scale` describes a rescaling, or zooming, of a CoordSysView.
-- | `viewLengths` is a scaling action centered at the middle of the
-- | view, such that the new view is set to `viewLengths` times the
-- | old view; `around` is like `viewLengths`, except centered at
-- | another point ({around: 0.5, viewLengths} == viewLengths);
-- | `viewRange` describes the edges of the new CoordSysView in terms
-- | of the old; a `viewRange` with (Pair 0.45 0.55) zooms in at the
-- | middle 10% of the view.
type Scale =
  Variant ( viewLengths :: Number
          , around      :: Around
          , viewRange   :: ViewRange )


data ViewChange =
    TranslateView Translation
  | ScaleView Scale
  | SetView CoordSysView







data Animation =
    Scrolling Number
  | Zooming   ViewRange
  | Jump



type Animator a =
     CoordSysView
  -> a
  -> Maybe { animation :: Animation
           , end       :: CoordSysView }

translateView :: (CoordSysView -> CoordSysView)
              -> Animator Translation
translateView clamp = V.case_



-- | A `ViewAnimation` describes either an animation that can be applied
-- | to achieve the *visual* effect required to stay in sync with the
-- | current change in view, or a final `CoordSysView` to apply to
-- | relevant tracks when the change in view is finished.
type ViewAnimation = Either Animation CoordSysView

animateDelta :: ∀ a a'.
                Monoid a'
             => (a' -> a -> a)
             -> (a -> Effect Unit)
             -> { current  :: Ref a
                , velocity :: Ref a'
                }
             -> Milliseconds
             -> Effect (a' -> Effect Unit)
animateDelta update done refs timeout = do

  comms <- onTimeout timeout do
    vD <- Ref.read refs.velocity
    Ref.write mempty refs.velocity
    launchAff_ $ liftEffect do
      Ref.modify (update vD) refs.current >>= done

  pure \cmd -> do
    Ref.modify_ (_ <> cmd) refs.velocity
    comms.run


type ViewControlOptions =
  { maxScrollTime :: Milliseconds
  , maxZoomTime :: Milliseconds }


defaultViewControlOptions =
  { maxScrollTime: wrap 200.0
  , maxZoomTime:   wrap 400.0 }


type ViewUpdateState =
  { start :: CoordSysView
  , end   :: CoordSysView
  , t     :: Number
  , totalAnimation :: Animation }


foldViewChange :: (CoordSysView -> CoordSysView)
               -> CoordSysView
               -> ViewChange
               -> ViewUpdateState
foldViewChange clamp' curView =
  let viewUpdate end anim = { start: curview, end, t: 0.0, totalAnimation: anim }
  in case _ of
       TranslateView t -> viewUpdate ?hm (Scrolling ?hm2)

       ScaleView     s -> viewUpdate ?hm (Zooming ?hm2)

       SetView     csv -> viewUpdate csv Jump



viewControl :: ∀ c.
               ViewControlOptions
            -> CoordSys c BigInt
            -> Maybe CoordSysView
            -> Milliseconds
            -> Effect { addListener :: (ViewAnimation -> Effect Unit) -> Effect Unit
                      , updateView  :: ViewChange -> Effect Unit }
viewControl opts cSys initView timeout = do

  let normView = normalizeView cSys (BigInt.fromInt 200000)

  currentView   <- Ref.new initView
  targetView    <- Ref.new initView

  currentAction <- Ref.new (Nothing :: Maybe ViewUpdateState)




  -- let updateView = case _ of



  pure $ unsafeCoerce unit


uiViewUpdate :: ∀ c r.
                CoordSys c BigInt
             -> Milliseconds
             -> Ref CoordSysView
             -> Effect Unit
             -> Effect (UpdateView -> Effect Unit)
uiViewUpdate cs timeout view cb = do
  let update vD = normalizeView cs (BigInt.fromInt 200000)
                 <<< updateViewFold vD
      done _ = cb

  velocity <- Ref.new mempty
  animateDelta update done { current: view, velocity } timeout
