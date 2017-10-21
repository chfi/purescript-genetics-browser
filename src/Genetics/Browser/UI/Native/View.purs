module Genetics.Browser.UI.Native.View where

import Prelude

import Data.Newtype (unwrap)
import Genetics.Browser.Units (Bp(..), BpPerPixel(..), bpToPixels, pixelsToBp)
import Graphics.Canvas (TranslateTransform, Transform)



type View = { min :: Bp
            , max :: Bp
            , scale :: BpPerPixel
            }

fromCanvasWidth :: Number
                -> { min :: Bp, max :: Bp }
                -> View
fromCanvasWidth w { min, max } = { min, max, scale }
  where scale = BpPerPixel $ (unwrap (max - min)) / w

browserTransform :: Number
                 -> Transform
browserTransform h = { m11: 1.0, m21:   0.0,  m31: 0.0
                     , m12: 0.0, m22: (-1.0), m32: h }


data UpdateView = ScrollBp Bp
                | ScrollPixels Number
                | SetRange Bp Bp
                | ModScale (BpPerPixel -> BpPerPixel)
                | SetScale BpPerPixel

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
