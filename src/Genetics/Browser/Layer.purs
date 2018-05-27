module Genetics.Browser.Layer where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse_)
import Graphics.Canvas as Canvas
import Unsafe.Coerce (unsafeCoerce)


type LayerPadding =
  { top  :: Number, bottom :: Number
  , left :: Number, right  :: Number }

data LayerType =
    Fixed
  | Scrolling

data LayerMask = NoMask | Masked

type Point = { x :: Number, y :: Number }

data Component a =
    Full   a
  | Padded a
  | Outside { top    :: a
            , right  :: a
            , bottom :: a
            , left   :: a }


derive instance eqComponent :: Eq a => Eq (Component a)
derive instance ordComponent :: Ord a => Ord (Component a)
derive instance functorComponent :: Functor Component
derive instance genericComponent :: Generic (Component a) _


data Layer a = Layer LayerType LayerMask (Component a)

type LayerDimensions =
  { size    :: Canvas.Dimensions
  , padding :: LayerPadding }

type ComponentSlot = { offset :: Point
                     , size   :: Canvas.Dimensions }

type CanvasComponent a = Component (ComponentSlot -> a)

layerSlots :: LayerDimensions
           -> _
layerSlots {size,padding} =
  let p0 = { x: 0.0, y: 0.0 }
      w  = size.width
      h  = size.height
      wC = w - padding.right - padding.left
      full   = { offset: p0
               , size }
      left   = { offset: p0
               , size: { width:  padding.left
                       , height: h
                       } }
      right  = { offset: p0 { x = w - padding.right }
               , size: { width:  padding.right
                       , height: h
                       } }
      top    = { offset: p0 { x = padding.left }
               , size: { width:  wC
                       , height: padding.top
                       } }
      bottom = { offset: { x:     padding.left
                         , y: h - padding.bottom }
               , size: { width:  wC
                       , height: padding.bottom
                       } }
      padded = { offset: { x: padding.left
                         , y: padding.top }
               , size: { width:  wC
                       , height: h - padding.top - padding.bottom
                       } }
  in { full, padded, top, right, bottom, left }


drawComponent :: ∀ a.
                 LayerDimensions
              -> Component (ComponentSlot -> a)
              -> Component a
drawComponent ld =
  let dims = layerSlots ld
  in case _ of
    Full    f -> Full    $ f dims.full
    Padded  f -> Padded  $ f dims.padded
    Outside f -> Outside $ { top:    f.top    dims.top
                           , right:  f.right  dims.right
                           , bottom: f.bottom dims.bottom
                           , left:   f.left   dims.left }


runLayer :: Layer (Canvas.Context2D -> Eff _ Unit)
         -> LayerDimensions
         -> Canvas.Context2D
         -> Eff _ Unit
runLayer (Layer lt lm c) dim ctx = do
  let slots = layerSlots dim
  case c of
    Full    f -> withComponent f lm ctx slots.full
    Padded  f -> withComponent f lm ctx slots.padded
    Outside f -> do
      withComponent f.top    lm ctx slots.top
      withComponent f.right  lm ctx slots.right
      withComponent f.bottom lm ctx slots.bottom
      withComponent f.left   lm ctx slots.left

  pure unit


withComponent :: (Canvas.Context2D -> Eff _ Unit)
              -> LayerMask
              -> Canvas.Context2D
              -> ComponentSlot
              -> Eff _ Unit
withComponent eff mask ctx {offset, size} = Canvas.withContext ctx do

  _ <- Canvas.transform
         { m11: 1.0, m21: 0.0, m31: offset.x
         , m12: 0.0, m22: 1.0, m32: offset.y } ctx

  case mask of
    NoMask -> pure unit
    Masked -> do

      _ <- Canvas.beginPath ctx
      _ <- Canvas.moveTo ctx 0.0        0.0
      _ <- Canvas.lineTo ctx size.width 0.0
      _ <- Canvas.lineTo ctx size.width size.height
      _ <- Canvas.lineTo ctx 0.0        size.height
      _ <- Canvas.lineTo ctx 0.0        0.0
      _ <- Canvas.clip ctx
      void $ Canvas.closePath ctx

  eff ctx
