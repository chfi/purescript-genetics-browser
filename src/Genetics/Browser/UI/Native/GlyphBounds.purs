module Genetics.Browser.UI.Native.GlyphBounds where

import Prelude

import Control.Monad.Free (foldFree)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Filterable (class Filterable, filter)
import Data.Identity (Identity(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Genetics.Browser.Glyph (Glyph)
import Genetics.Browser.GlyphF (GlyphF(..))
import Genetics.Browser.Types (Point)
import Math as Math

newtype GlyphBounds = GlyphBounds (Point -> Boolean)

derive instance newtypeGlyphBounds :: Newtype GlyphBounds _

instance semigroupGlyphBounds :: Semigroup GlyphBounds where
  append (GlyphBounds a) (GlyphBounds b) = GlyphBounds (a || b)

instance monoidGlyphBounds :: Monoid GlyphBounds where
  mempty = GlyphBounds $ const false

glyphBoundsNat :: GlyphF ~> Writer GlyphBounds
glyphBoundsNat (Circle p r a) = do
  tell $ GlyphBounds \p' -> let x' = p'.x - p.x
                                y' = p'.y - p.y
                            in Math.sqrt ((x' * x') + (y' * y')) < 100.0
  pure a
glyphBoundsNat (Line _ _ a) = pure a
glyphBoundsNat (Rect p1 p2 a) = do
  tell $ GlyphBounds \p' -> (p'.x > p1.x && p'.x < p2.x) &&
                            (p'.y > p1.y && p'.y < p2.y)
  pure a
glyphBoundsNat (Stroke _ a) = pure a
glyphBoundsNat (Fill _ a) = pure a
glyphBoundsNat (Path _ a) = pure a


glyphBounds :: forall a. Glyph a -> GlyphBounds
glyphBounds = execWriter <<< foldFree glyphBoundsNat


-- Filters a collection of glyphs annotated with some functor,
-- returning glyphs that cover the given point on the canvas
clickAnnGlyphs :: forall f g a.
                  Filterable f
               => Functor g
               => Eq (g Boolean)
               => f (g (Glyph a))
               -> Point
               -> f (g (Glyph a))
clickAnnGlyphs gs p =
    filter (\x -> map pred x == map (const true) x) gs
  where pred = \g -> unwrap (glyphBounds g) p

-- For collections of unannotated glyphs
clickGlyphs :: forall f a.
               Filterable f
            => f (Glyph a)
            -> Point
            -> f (Glyph a)
clickGlyphs fs p = unwrap <$> clickAnnGlyphs (map Identity fs) p
