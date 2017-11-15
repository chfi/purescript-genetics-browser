module Genetics.Browser.View where

import Prelude

import Data.Array (index, (..))
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF, unwrap, wrap)
import Data.Traversable (traverse)
import Genetics.Browser.Types (Bp(..), BpPerPixel(..), Chr, Point, Pos, Range)
import Graphics.Canvas (TranslateTransform, Transform)



-- type Scale r = { scale :: BpPerPixel | r }

-- type PixelsWide r = { pixels :: Number | r }

-- type ChrSize r = { chrSize :: Chr -> Number | r }

-- type View r = Range (Scale (PixelsWide r))

type Pixels = Number

type View = { lHand :: Pos, rHand :: Pos
            , scale :: BpPerPixel
            , pixelsWide :: Pixels
            }

-- TODO This module is probably not the best place for these functions
chrsInRange :: forall aff a r.
               Array Chr
            -> Range r
            -> Maybe (Array Chr)
chrsInRange chrs { lHand, rHand } = do
  lI <- Array.findIndex (\x -> x.chrId == lHand.chrId) chrs
  rI <- Array.findIndex (\x -> x.chrId == rHand.chrId) chrs
  traverse (index chrs) (lI .. rI)


getRangeSize :: forall aff a r.
                Array Chr
             -> Range r
             -> Maybe Bp
getRangeSize chrs r@{ lHand, rHand } = do
  chrs' <- chrsInRange chrs r
  {head, tail} <- Array.uncons chrs'
  {init, last} <- Array.unsnoc tail

  let l = head.size - lHand.bp
      mid = alaF Additive foldMap (_.size) init
      r = rHand.bp
  pure $ l + mid + r


-- we can just assume we always have all the data...
-- only special instances when that's not the case, e.g. when creating it

-- newtype View =


fromCanvasWidth :: forall r.
                   Array Chr
                -> { lHand :: Pos, rHand :: Pos
                   , pixelsWide :: Pixels }
                -> Maybe View
fromCanvasWidth chrs v' = do
  totalBps <- getRangeSize chrs v'
  let scale = wrap $ (unwrap totalBps) / v'.pixelsWide

  pure $ { lHand: v'.lHand, rHand: v'.rHand, pixelsWide: v'.pixelsWide, scale: scale }


browserTransform :: Number
                 -> Transform
browserTransform h = { m11: 1.0, m21:   0.0,  m31: 0.0
                     , m12: 0.0, m22: (-1.0), m32: h }


data UpdateView = ScrollBp Bp
                | ScrollPixels Number
                | SetRange (forall r. Range r)
                | ModScale (BpPerPixel -> BpPerPixel)
                | SetScale BpPerPixel

-- scaleToFit :: forall r.
--               Range (PixelsWide r)
--            -> View r
-- scaleToFit r@{ lHand, rHand, pixels } =
--   r { scale = rHand.}



-- fitToScale :: forall r.
--               Scale (PixelsWide r)
--            -> View r
-- fitToScale { scale, pixels } = ?pas


-- this should be a composition of just applying `f` then `fitToScale` or similar
modScale :: View
         -> (BpPerPixel -> BpPerPixel)
         -> View
modScale v f = v
  -- where bpsWide = v.max - v.min
  --       pixelsWide = bpToPixels v.scale bpsWide
  --       mid = v.min + ((v.max - v.min) * Bp 0.5)
  --       scale = f v.scale
  --       bpsWide' = pixelsToBp scale pixelsWide
  --       d = bpsWide' * Bp 0.5
{-
foldView :: UpdateView
         -> View
         -> View
foldView (ScrollBp x)     v = v { min = v.min + x
                                , max = v.max + x }

foldView (ScrollPixels x) v = let dx = pixelsToBp v.scale x
                              in v { min = v.min - dx
                                   , max = v.max - dx }

foldView (SetRange l r)   v = v { min = l, max = r }

foldView (ModScale f)     v = modScale v f

foldView (SetScale s)     v = modScale v (const s)

-}
