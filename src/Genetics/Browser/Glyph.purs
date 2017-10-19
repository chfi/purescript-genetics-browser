module Genetics.Browser.Glyph
       ( Glyph
       , circle, line, rect, stroke, fill, path
       )
       where

import Control.Monad.Free (Free, liftF)
import Genetics.Browser.GlyphF (GlyphF(..))
import Genetics.Browser.Types (Point)
import Prelude

{-
TODO: could we use FreeAp instead?
instead of e.g. setting stroke & fill and keeping it in state,
explicitly tagging circles etc. (if that's even necessary).

the idea is that if we could do more introspection we could batch
together all circles of a given color; when drawing 1000s of glyphs,
setting a color once instead of 1000s of times makes a difference.

however, it's possible that could be done with a Free monad too -- need to read more.
-}

-- | Free monad for constructing glyphs
type Glyph = Free GlyphF

circle :: Point -> Number -> Glyph Unit
circle p r = liftF $ Circle p r unit

line :: Point -> Point -> Glyph Unit
line p1 p2 = liftF $ Line p1 p2 unit

rect :: Point -> Point -> Glyph Unit
rect p1 p2 = liftF $ Rect p1 p2 unit

stroke :: String -> Glyph Unit
stroke c = liftF $ Stroke c unit

fill :: String -> Glyph Unit
fill c = liftF $ Fill c unit

path :: Array Point -> Glyph Unit
path ps = liftF $ Path ps unit
