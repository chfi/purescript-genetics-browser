module Genetics.Browser.GlyphF
       ( GlyphF(..)
       )
       where

import Prelude

import Genetics.Browser.Types (Point)

data GlyphF a =
    Circle Point Number a
  | Line Point Point a
  | Rect Point Point a
  | Stroke String a
  | Fill String a
  | Path (Array Point) a

derive instance functorGlyph :: Functor GlyphF
