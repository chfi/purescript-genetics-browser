module Genetics.Browser.Layer where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, sequenceDefault, traverse_)
import Graphics.Canvas as Canvas
import Unsafe.Coerce (unsafeCoerce)


type LayerPadding =
  { top  :: Number, bottom :: Number
  , left :: Number, right  :: Number }

data LayerType =
    Fixed
  | Scrolling

data LayerMask = NoMask | Masked

derive instance eqLayerMask :: Eq LayerMask


type Point = { x :: Number, y :: Number }

data Component a =
    Full   a
  | Padded Number a
  | Outside { top    :: a
            , right  :: a
            , bottom :: a
            , left   :: a }


derive instance eqComponent :: Eq a => Eq (Component a)
derive instance ordComponent :: Ord a => Ord (Component a)
derive instance functorComponent :: Functor Component
derive instance genericComponent :: Generic (Component a) _

instance foldableComponent :: Foldable Component where
  foldMap :: ∀ a m. Monoid m => (a -> m) -> Component a -> m
  foldMap f = case _ of
    Full     a -> f a
    Padded _ a -> f a
    Outside as -> f as.top <> f as.right <> f as.bottom <> f as.left

  foldr f i c = foldrDefault f i c
  foldl f i c = foldlDefault f i c

instance traverseComponent :: Traversable Component where
  traverse :: ∀ a b m. Applicative m => (a -> m b) -> Component a -> m (Component b)
  traverse f = case _ of
    Full     a -> Full <$> f a
    Padded r a -> Padded r <$> f a
    Outside as ->
      (\t r b l -> Outside {top: t, right: r, bottom: b, left: l})
      <$> f as.top
      <*> f as.right
      <*> f as.bottom
      <*> f as.left

  sequence t = sequenceDefault t

data Layer a = Layer LayerType LayerMask (Component a)

type LayerDimensions =
  { size    :: Canvas.Dimensions
  , padding :: LayerPadding }

type ComponentSlot = { offset :: Point
                     , size   :: Canvas.Dimensions }

type CanvasComponent a = Component (ComponentSlot -> a)

type LayerSlots a =
  ( full   :: a
  , padded :: a
  , top    :: a
  , right  :: a
  , bottom :: a
  , left   :: a )

layerSlots :: LayerDimensions
           -> Record (LayerSlots ComponentSlot)
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
    Full     f -> Full     $ f dims.full
    Padded p f -> Padded p $ f dims.padded
    Outside  f -> Outside  $ { top:    f.top    dims.top
                             , right:  f.right  dims.right
                             , bottom: f.bottom dims.bottom
                             , left:   f.left   dims.left }
