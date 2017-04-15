module Genetics.Browser.GlyphF.Canvas
       ( renderGlyph )
       where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Free (foldFree)
import Data.Traversable (traverse_)
import Genetics.Browser.Glyph (Glyph)
import Genetics.Browser.GlyphF (GlyphF(..))
import Graphics.Canvas (CANVAS, Context2D)
import Graphics.Canvas as C
import Math as Math


-- TODO would be nice not to have to throw the contexts away...
glyphEffN :: ∀ eff. Context2D -> GlyphF ~> Eff (canvas :: CANVAS | eff)
glyphEffN ctx (Stroke c a) = do
  _ <- C.setStrokeStyle c ctx
  pure a
glyphEffN ctx (Fill c a) = do
  _ <- C.setFillStyle c ctx
  pure a
glyphEffN ctx (Circle p r a) = do
  _ <- C.beginPath ctx
  _ <- C.arc ctx { x: p.x
            , y: p.y
            , r: r
            , start: 0.0
            , end: 2.0 * Math.pi
            }
  _ <- C.stroke ctx
  _ <- C.fill ctx
  pure a
glyphEffN ctx (Line p1 p2 a) = do
  _ <- C.moveTo ctx p1.x p1.y
  _ <- C.lineTo ctx p2.x p2.y
  _ <- C.stroke ctx
  pure a
glyphEffN ctx (Rect p1 p2 a) = do
  let r = { x: p1.x
          , y: p1.y
          , w: p2.x - p1.x
          , h: p2.y - p1.y
          }
  _ <- C.fillRect ctx r
  _ <- C.strokeRect ctx r
  pure a
glyphEffN ctx (Path ps a) = do
  _ <- C.beginPath ctx
  traverse_ (\p -> C.lineTo ctx p.x p.y) ps
  _ <- C.stroke ctx
  pure a


renderGlyph :: ∀ eff. Context2D -> Glyph ~> Eff (canvas :: CANVAS | eff)
renderGlyph = foldFree <<< glyphEffN
