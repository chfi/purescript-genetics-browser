module Genetics.Browser.GlyphPosition
       ( GlyphPosition(..) )
       where

import Data.Generic.Rep (class Generic)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Global (infinity)
import Math as Math
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)


-- this is really a silly way of checking whether the user has clicked on a glyph or not.
-- instead of simply using rectangular bounding boxes (encoded with constant functions??)
-- we should have a general collide(x,y) function. that function should return true if
-- (x,y) is within the glyph, and false otherwise. How it is calculated is up to the glyph.

-- for example a haplotype plot would use a simple rectangle check, but a QTL plot could
-- check to see if the click was within some radius of the data point (right now lineplots
-- can't really have positions!)

-- | GlyphPositions are used by Biodalliance to calculate the hitbox for a glyph,
-- | for seeing if the user has clicked on it.
-- | Easily combined using the Monoid instance.
newtype GlyphPosition = GlyphPos { min :: Number
                                 , max :: Number
                                 , minY :: Number
                                 , maxY :: Number
                                 }

derive instance genericGlyphPosition :: Generic GlyphPosition _
derive instance newtypeGlyphPosition :: Newtype GlyphPosition _
derive instance eqGlyphPosition :: Eq GlyphPosition

instance arbitraryGlyphPosition :: Arbitrary GlyphPosition where
  arbitrary = do
    min <- arbitrary
    max <- arbitrary
    minY <- arbitrary
    maxY <- arbitrary
    pure $ GlyphPos { min, max, minY, maxY }

instance showGlyphPosition :: Show GlyphPosition where
  show (GlyphPos (gp)) = "{ min: "  <> show gp.min  <>
                         ", max: "  <> show gp.max  <>
                         ", minY: " <> show gp.minY <>
                         ", maxY: " <> show gp.maxY <>
                         " }"


instance semigroupGlyphPosition :: Semigroup GlyphPosition where
  append (GlyphPos (p1)) (GlyphPos (p2)) =
    GlyphPos ({ min: Math.min  p1.min  p2.min
              , max: Math.max  p1.max  p2.max
              , minY: Math.min p1.minY p2.minY
              , maxY: Math.max p1.maxY p2.maxY
              })


instance monoidGlyphPosition :: Monoid GlyphPosition where
  mempty = GlyphPos { min:  infinity
                    , max:  (-infinity)
                    , minY: infinity
                    , maxY: (-infinity)
                    }
