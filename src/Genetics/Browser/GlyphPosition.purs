module Genetics.Browser.GlyphPosition where
       -- ( GlyphPosition(..)
       -- )
       -- where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Global (infinity)
import Math as Math


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


                   -- GlyphPos { min, max, minY, maxY }

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


newtype BBox a = BBox { min :: Maybe (Min a)
                      , max :: Maybe (Max a)
                      , minY :: Maybe (Min a)
                      , maxY :: Maybe (Max a)
                      }

instance functorBBox :: Functor BBox where
  map f (BBox b) = BBox $ { min:  map (over Min f) b.min
                          , max:  map (over Max f) b.max
                          , minY: map (over Min f) b.minY
                          , maxY: map (over Max f) b.maxY
                          }

instance semigroupBBox :: Ord a => Semigroup (BBox a) where
  append (BBox b1) (BBox b2) =
    BBox { min:  b1.min  <> b2.min
         , max:  b1.max  <> b2.max
         , minY: b1.minY <> b2.minY
         , maxY: b1.maxY <> b2.maxY
         }

instance monoidBBox :: Ord a => Monoid (BBox a) where
  mempty = BBox { min: Nothing
                , max: Nothing
                , minY: Nothing
                , maxY: Nothing
                }

mkBB :: Number -> Number -> Number -> Number -> BBox Number
mkBB min max minY maxY = BBox { min: pure $ wrap min
                              , max: pure $ wrap max
                              , minY: pure $ wrap min
                              , maxY: pure $ wrap max
                              }
