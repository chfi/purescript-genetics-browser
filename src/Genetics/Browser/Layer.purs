module Genetics.Browser.Layer where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (log)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Lens (Getter', to, (^.))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, sequenceDefault, traverse_)
import Data.Variant (Variant, case_, onMatch)
import Graphics.Canvas (CanvasElement, Context2D)
import Graphics.Canvas as Canvas
import Type.Prelude (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import setContextTranslation :: ∀ e.
                                        Point
                                     -> Context2D
                                     -> Eff e Unit

type Point = { x :: Number, y :: Number }

type BrowserPadding =
  { top  :: Number, bottom :: Number
  , left :: Number, right  :: Number }

type BrowserDimensions =
  { size    :: Canvas.Dimensions
  , padding :: BrowserPadding }

data Component a =
    Full   a
  | Padded Number a
  | CTop a
  | CRight a
  | CBottom a
  | CLeft a

_Component :: ∀ a. Getter' (Component a) a
_Component = to case _ of
  Full     a -> a
  Padded _ a -> a
  CTop     a -> a
  CRight   a -> a
  CBottom  a -> a
  CLeft    a -> a

derive instance eqComponent :: Eq a => Eq (Component a)
derive instance ordComponent :: Ord a => Ord (Component a)
derive instance functorComponent :: Functor Component
derive instance genericComponent :: Generic (Component a) _

instance foldableComponent :: Foldable Component where
  foldMap :: ∀ a m. Monoid m => (a -> m) -> Component a -> m
  foldMap f c = f $ c ^. _Component
  foldr f i c = foldrDefault f i c
  foldl f i c = foldlDefault f i c

instance traverseComponent :: Traversable Component where
  traverse :: ∀ a b m. Applicative m => (a -> m b) -> Component a -> m (Component b)
  traverse f = case _ of
    Full     a -> Full     <$> f a
    Padded r a -> Padded r <$> f a
    CTop     a -> CTop     <$> f a
    CRight   a -> CRight   <$> f a
    CBottom  a -> CBottom  <$> f a
    CLeft    a -> CLeft    <$> f a

  sequence t = sequenceDefault t

data Layer a = Layer LayerType LayerMask (Component a)

data LayerType =
    Fixed
  | Scrolling

data LayerMask = NoMask | Masked

derive instance eqLayerMask :: Eq LayerMask

type ComponentSlot = { offset :: Point
                     , size   :: Canvas.Dimensions }

type BrowserSlots a =
  ( full   :: a
  , padded :: a
  , top    :: a
  , right  :: a
  , bottom :: a
  , left   :: a )

_full   = SProxy :: SProxy "full"
_padded = SProxy :: SProxy "padded"
_top    = SProxy :: SProxy "top"
_right  = SProxy :: SProxy "right"
_bottom = SProxy :: SProxy "bottom"
_left   = SProxy :: SProxy "left"


slotOffset :: ∀ a.
              BrowserDimensions
           -> Point
           -> Variant (BrowserSlots a)
           -> Point
slotOffset {size, padding} p = case_ # onMatch
  { full:   \_ -> p
  , padded: \_ -> { x: p.x + padding.left, y: p.y + padding.top }
  , top:    \_ -> { x: p.x + padding.left, y: p.y }
  , right:  \_ -> { x: p.x + ( size.width - padding.right ), y: p.y }
  , bottom: \_ -> { x: p.x + padding.left, y: p.y + ( size.height - padding.bottom ) }
  , left:   \_ -> p
  }

browserSlots :: BrowserDimensions
             -> Record (BrowserSlots ComponentSlot)
browserSlots {size,padding} =
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





-- | Provided a component slot (contents irrelevant), and the
-- | dimensions & padding of the browser, `slotContext` provides a
-- | canvas context that has been translated to the relevant slot
slotContext :: ∀ m a.
                MonadEff _ m
             => Component a
             -> BrowserDimensions
             -> CanvasElement
             -> m Context2D
slotContext com dims el = liftEff do

  ctx <- Canvas.getContext2D el

  let slots = browserSlots dims

  -- TODO handle masking etc.
  case com of
    Full     _ -> pure ctx
    Padded r _ -> do
      -- the `r` padding is *not* part of the BrowserDimensions; it's just cosmetic
      setContextTranslation { x: slots.padded.offset.x + r
                            , y: slots.padded.offset.y + r } ctx
      pure ctx
    CTop     _ -> do
      setContextTranslation { x: slots.top.offset.x
                            , y: slots.top.offset.y } ctx
      pure ctx
    CRight   _ -> do
      setContextTranslation { x: slots.right.offset.x
                            , y: slots.right.offset.y } ctx
      pure ctx
    CBottom  _ -> do
      setContextTranslation { x: slots.bottom.offset.x
                            , y: slots.bottom.offset.y } ctx
      pure ctx
    CLeft    _ -> do
      setContextTranslation { x: slots.left.offset.x
                            , y: slots.left.offset.y } ctx
      pure ctx
