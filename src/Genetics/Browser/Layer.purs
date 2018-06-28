module Genetics.Browser.Layer where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Getter', to)
import Data.Variant (Variant, case_, inj, onMatch)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (CanvasElement, Context2D, beginPath, clip, closePath, lineTo, moveTo)
import Graphics.Canvas as Canvas
import Type.Prelude (SProxy(..))


foreign import setContextTranslation :: Point
                                     -> Context2D
                                     -> Effect Unit

type Point = { x :: Number, y :: Number }

type TrackPadding =
  { top  :: Number, bottom :: Number
  , left :: Number, right  :: Number }

type TrackDimensions =
  { size    :: Canvas.Dimensions
  , padding :: TrackPadding }

data Component a =
    Full   a
  | Center a
  | CTop a
  | CRight a
  | CBottom a
  | CLeft a

_Component :: ∀ a. Getter' (Component a) a
_Component = to case _ of
  Full     a -> a
  Center   a -> a
  CTop     a -> a
  CRight   a -> a
  CBottom  a -> a
  CLeft    a -> a

derive instance eqComponent :: Eq a => Eq (Component a)
derive instance ordComponent :: Ord a => Ord (Component a)
derive instance functorComponent :: Functor Component
derive instance genericComponent :: Generic (Component a) _

data Layer a = Layer LayerType LayerMask (Component a)

derive instance functorLayer :: Functor Layer


data LayerType =
    Fixed
  | Scrolling

data LayerMask = NoMask | Masked Number

derive instance eqLayerMask :: Eq LayerMask

type ComponentSlot = { offset :: Point
                     , size   :: Canvas.Dimensions }

type TrackSlots a =
  ( full   :: a
  , center :: a
  , top    :: a
  , right  :: a
  , bottom :: a
  , left   :: a )

_full   = SProxy :: SProxy "full"
_center = SProxy :: SProxy "center"
_top    = SProxy :: SProxy "top"
_right  = SProxy :: SProxy "right"
_bottom = SProxy :: SProxy "bottom"
_left   = SProxy :: SProxy "left"

asSlot :: ∀ a. Component a -> Variant (TrackSlots a)
asSlot = case _ of
  Full     a -> inj _full   a
  Center   a -> inj _center a
  CTop     a -> inj _top    a
  CRight   a -> inj _right  a
  CBottom  a -> inj _bottom a
  CLeft    a -> inj _left   a

slot :: ∀ a.
        TrackDimensions
     -> Variant (TrackSlots a)
     -> ComponentSlot
slot {size, padding} =
  let p0 = { x: 0.0, y: 0.0 }
      w  = size.width
      h  = size.height
      wC = w - padding.right - padding.left
  in case_ # onMatch
       { full:   \_ -> { offset: p0, size }

       , center: \_ -> { offset: { x: padding.left
                                 , y: padding.top }
                       , size: { width:  wC
                               , height: h - padding.top - padding.bottom
                               } }

       , top:    \_ -> { offset: p0 { x = padding.left }
                       , size: { width:  wC
                               , height: padding.top
                               } }

       , right:  \_ ->  { offset: { x: w - padding.right
                                  , y: padding.top }
                        , size: { width:  padding.right
                                , height: h - (padding.top + padding.bottom)
                                } }

       , bottom: \_ ->   { offset: { x:     padding.left
                                   , y: h - padding.bottom }
                         , size: { width:  wC
                                 , height: padding.bottom
                                 } }

       , left:   \_ ->  { offset: p0 { y = padding.top }
                        , size: { width:  padding.left
                                , height: h - (padding.top + padding.bottom)
                                } }

       }


slotOffset :: ∀ a.
              TrackDimensions
           -> Variant (TrackSlots a)
           -> Point
slotOffset sp = _.offset <<< slot sp

slotSize :: ∀ a.
            TrackDimensions
         -> Variant (TrackSlots a)
         -> Canvas.Dimensions
slotSize sp = _.size <<< slot sp


trackSlots :: TrackDimensions
            -> Record (TrackSlots ComponentSlot)
trackSlots sp =
  { full:   slot sp $ inj _full unit
  , center: slot sp $ inj _center unit
  , top:    slot sp $ inj _top unit
  , right:  slot sp $ inj _right unit
  , bottom: slot sp $ inj _bottom unit
  , left:   slot sp $ inj _left unit
  }



-- | Provided a component slot (contents irrelevant), and the
-- | dimensions & padding of the track, `slotContext` provides a
-- | canvas context that has been translated to the relevant slot
slotContext :: ∀ m a.
                MonadEffect m
             => Layer a
             -> TrackDimensions
             -> CanvasElement
             -> m Context2D
slotContext (Layer _ mask com) dims el = liftEffect do

  ctx <- Canvas.getContext2D el

  let slots = trackSlots dims
      clipMask p0 p1 = do
        _ <- beginPath ctx
        _ <- moveTo ctx p0.x p0.y
        _ <- lineTo ctx p1.x p0.y
        _ <- lineTo ctx p1.x p1.y
        _ <- lineTo ctx p0.x p1.y
        _ <- clip ctx
        void $ closePath ctx

  let offset = slotOffset dims (asSlot com)
      size = slotSize dims (asSlot com)

  setContextTranslation offset ctx

  case mask of
    NoMask -> pure unit
    Masked r -> clipMask { x: -r, y: -r }
                         { x: size.width  + r
                         , y: size.height + r }

  pure ctx
