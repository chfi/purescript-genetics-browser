module Genetics.Browser.View where

import Prelude

import Data.Newtype (unwrap)
import Genetics.Browser.Units (Bp(..), BpPerPixel(..), bpToPixels, pixelsToBp)
import Graphics.Canvas (TranslateTransform, Transform)
import Genetics.Browser.Types (Point, Pos)


type Range r = { lHand :: Pos, rHand :: Pos  | r}

-- type Scale r = { scale :: BpPerPixel | r }

-- type PixelsWide r = { pixels :: Number | r }

-- type ChrSize r = { chrSize :: Chr -> Number | r }

-- type View r = Range (Scale (PixelsWide r))

type Pixels = Number

type View = { lHand :: Pos, rHand :: Pos
            , scale :: BpPerPixel
            , pixelsWide :: Pixels
            }


-- we can just assume we always have all the data...
-- only special instances when that's not the case, e.g. when creating it

-- newtype View =


fromCanvasWidth :: (Chr -> Bp)
                -> { lHand :: Pos, rHand :: Pos
                   , pixelsWide :: Pixels }
                -> View
fromCanvasWidth chrSizes v' = v' { scale = scale }
  -- TODO fix this...
  -- sum up the chr sizes in between if applicable
  where scale = BpPerPixel $ (unwrap (max - min)) / w


browserTransform :: Number
                 -> Transform
browserTransform h = { m11: 1.0, m21:   0.0,  m31: 0.0
                     , m12: 0.0, m22: (-1.0), m32: h }


data UpdateView = ScrollBp Bp
                | ScrollPixels Number
                | SetRange (Range ())
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



modScale :: View
         -> (BpPerPixel -> BpPerPixel)
         -> View
modScale v f = { scale, min: mid - d, max: mid + d }
  where bpsWide = v.max - v.min
        pixelsWide = bpToPixels v.scale bpsWide
        mid = v.min + ((v.max - v.min) * Bp 0.5)
        scale = f v.scale
        bpsWide' = pixelsToBp scale pixelsWide
        d = bpsWide' * Bp 0.5

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
