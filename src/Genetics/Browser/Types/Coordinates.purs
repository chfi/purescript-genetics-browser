module Genetics.Browser.Types.Coordinates where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (any, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Lens (Getter', Lens, _2, findOf, folded, to, view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, alaF, unwrap)
import Data.Pair (Pair(..))
import Data.Tuple (Tuple(Tuple))
import Genetics.Browser.Types (Point)


-- | The global coordinate system works by taking the sum of the chromosome sizes,
-- | for now mainly represented as a BigInt, as an Int representation may overflow.
-- | Subsets of the global coordinate system are defined by some interval, namely a Pair of Ints


-- | Helper function to calculate the size of the stretch covered by a pair.
pairSize :: forall c.
            Ring c
         => Pair c
         -> c
pairSize (Pair l r) = r - l


-- | Helper function for defining predicates on pairs that contain a given point.
inPair :: forall c.
          Ord c
       => c
       -> Pair c
       -> Boolean
inPair p (Pair l r) = l <= p && p <= r


-- TODO test this one

-- | Helper function for defining predicates on pairs that overlap with a given pair.
pairsOverlap :: forall c.
                Ord c
             => Pair c
             -> Pair c
             -> Boolean
pairsOverlap (Pair l1 r1) (Pair l2 r2) =
  let (Pair l1' r1') = Pair (min l1 l2) (min r1 r2)
      (Pair l2' r2') = Pair (max l1 l2) (max r1 r2)
  in r1' >= l2'


-- | Newtype for representing values normalized to fit in some range
newtype Normalized a = Normalized a

derive instance newtypeNormalized :: Newtype (Normalized a) _

normalizePoint :: { width :: Number
                  , height :: Number }
               -> Point
               -> Normalized Point
normalizePoint s {x, y} = Normalized { x: x', y: y' }
  where x' = x / s.width
        y' = y / s.height

rescalePoint :: { width :: Number, height :: Number }
             -> Normalized Point
             -> Point
rescalePoint s (Normalized p) = {x: p.x * s.width, y: p.y * s.height}


-- this should be in the UI, actually.
-- the padding for the view is calculated from the current scale & is in [minPixels, maxPixels]
type SegmentPadding = { minPixels :: Number
                      , maxPixels :: Number }


-- | A coordinate system is defined by a set of segments,
-- | which are defined by (mutually exclusive and contiguous) intervals
-- | over the browser coordinates (BigInt in most cases)
newtype CoordSys i c =
  CoordSys (Segments i c)


-- | A single segment is the segment identifier together with the range it covers.
type Segment i c  = Tuple i (Pair c)

-- TODO the `Map i (Pair c)` definition of `Segments i c` makes it
--      tricky to work with `Segments` using lenses; having problems
--      working with whole `Tuple i (Pair c)` from `Map i (Pair c)`
--      without using Map.toUnfoldable, which requires an annotation...

-- | Several segments are indexed by their identifiers.
type Segments i c = Map i (Pair c)


-- | The `Functor` instance maps over the segment borders.
instance functorCoordSys :: Functor (CoordSys i) where
  map f (CoordSys cs) = CoordSys $ (map <<< map) f cs

derive instance newtypeCoordSys :: Newtype (CoordSys i c) _
derive instance genericCoordSys :: Generic (CoordSys i c) _

-- | Lens to the segments in a coordinate system.
_Segments :: forall i c j d.
             Lens
             (CoordSys i c) (CoordSys j d)
             (Segments i c) (Segments j d)
_Segments = _Newtype


-- | A Getter' to retrieve the total size of the coordinate system by summing its parts.
_TotalSize :: forall i c.
              Ring c
           => Getter' (CoordSys i c) c
_TotalSize = _Segments
             <<< (to $ alaF Additive foldMap pairSize)


-- | A Getter' into the size of a segment.
_SegmentSize :: forall i c.
                Ring c
             => Getter' (Segment i c) c
_SegmentSize = _2 <<< to pairSize



-- | A coordinate system is created by providing an array of pairs of
-- | chromosome/segment name (often in the ChrId newtype) and their
-- | respective sizes.
coordSys :: forall i c.
            Ord i
         => Semiring c
         => Array (Tuple i c)
         -> CoordSys i c
coordSys segs = CoordSys $ Map.fromFoldable
                $ Array.zip ids (offsets sizes)
  where (Tuple ids sizes) = Array.unzip segs

        -- Given an array of sizes, produce an array of contiguous intervals.
        offsets :: Array c -> Array (Pair c)
        offsets xs = let os = Array.scanl (\x y -> x + y) zero xs
                     in Array.zipWith Pair (zero `Array.cons` os) os


-- | Given a coordinate system and a point, find the segment that contains the point, if any.
lookupSegment :: forall i c.
                 Ord c
              => CoordSys i c
              -> c
              -> Maybe (Segment i c)
lookupSegment cs x = findOf folded pred array
  where pred :: Segment i c -> Boolean
        pred = view (_2 <<< to (inPair x))
        array :: Array (Segment i c)
        array = cs ^. _Segments <<< to Map.toUnfoldable


-- | Given a coordinate system and a range, find the segments that overlap, even partially, with the range.
segmentsInPair :: forall i c.
                  Ord i
               => Ord c
               => CoordSys i c
               -> Pair c
               -> Segments i c
segmentsInPair cs x = Map.filter (any (_ `inPair` x)) $ cs ^. _Segments



newtype ViewScale = ViewScale { pixelWidth :: Number, coordWidth :: BigInt }

derive instance eqViewScale :: Eq ViewScale

newtype CoordSysView = CoordSysView (Pair BigInt)

derive instance coordsysviewNewtype :: Newtype CoordSysView _

setViewWidth :: BigInt
             -> Pair BigInt
             -> Pair BigInt
setViewWidth w p@(Pair l r) =
  let width = pairSize p
      rad = width / BigInt.fromInt 2
      mid = l + rad
  in Pair (mid - w) (mid + r)

normalizeView :: forall i.
                 CoordSys i BigInt
              -> BigInt
              -> CoordSysView
              -> CoordSysView
normalizeView cs minWidth csv =
  let limL = zero
      limR = cs ^. _TotalSize
      p = unwrap csv
      len = max minWidth (pairSize p)
      (Pair l r) = setViewWidth len p
      vr' = case l < limL, r > limR of
              true, false  -> Pair limL len
              false, true  -> Pair (limR - len) limR
              true, true   -> Pair limL limR
              false, false -> Pair l r
  in CoordSysView vr'



viewScale :: forall r.
             { width :: Number | r } -> CoordSysView -> ViewScale
viewScale {width} (CoordSysView csView) =
  let coordWidth = pairSize csView
  in ViewScale { pixelWidth: width, coordWidth }

-- | The scale of the browser view is defined by how much of the coordinate system is visible,
-- | and how big the screen it must fit into.
type Scale = { screenWidth :: Number, viewWidth :: BigInt }

-- | Given the width of the display in pixels, and how much of the coordinate system
-- | that is currently visible, scale a point in the coordinate system
-- | to the screen.
-- | Always uses zero as the origin -- points scaled with this must still be translated!
scaleToScreen :: Scale
              -> BigInt
              -> Number
scaleToScreen {screenWidth, viewWidth} x =
  BigInt.toNumber x * (screenWidth / (BigInt.toNumber viewWidth))

scaleToScreen' :: ViewScale
               -> BigInt
               -> Number
scaleToScreen' (ViewScale {pixelWidth, coordWidth}) x =
  BigInt.toNumber x * (pixelWidth / (BigInt.toNumber coordWidth))


-- | Given the width of the display in pixels, and how much of the coordinate system
-- | that is currently visible, scale a point given in pixels from the left-hand edge
-- | to browser units from the beginning of the browser.
-- | Always uses zero as the origin -- points scaled with this must still be translated!
scaleToGlobal :: Scale
              -> Number
              -> BigInt
scaleToGlobal {screenWidth, viewWidth} x =
  BigInt.fromNumber $ x * ((BigInt.toNumber viewWidth) / screenWidth)

scaleToGlobal' :: ViewScale
              -> Number
              -> BigInt
scaleToGlobal' (ViewScale {pixelWidth, coordWidth}) x =
  BigInt.fromNumber $ x * ((BigInt.toNumber coordWidth) / pixelWidth)


pixelsView :: ViewScale -> CoordSysView -> Pair Number
pixelsView vs (CoordSysView (Pair l r)) =
  let f = scaleToScreen' vs
  in Pair (f l) (f r)

-- | Given a coordinate system and browser scale,
-- | return the browser segments scaled to canvas coordinates.
scaledSegments :: forall i.
                  CoordSys i BigInt
               -> Scale
               -> Segments i Number
scaledSegments cs scale = (map <<< map) (scaleToScreen scale) $ cs ^. _Segments

scaledSegments' :: forall i.
                  CoordSys i BigInt
               -> ViewScale
               -> Segments i Number
scaledSegments' cs scale = (map <<< map) (scaleToScreen' scale) $ cs ^. _Segments


-- | Helper functions for translating and scaling pairs.
-- | They're concretized to `BigInt` for convenience; in truth they would
-- | probably fit better in the UI module.

-- | Translate an BigInt pair to the right by `x` times its size.
translatePairBy :: Pair BigInt
                -> Number
                -> Pair BigInt
translatePairBy p x = (_ + delta) <$> p
  where delta = BigInt.fromNumber $ x * (BigInt.toNumber $ pairSize p)

translateViewBy :: CoordSysView
                -> Number
                -> CoordSysView
translateViewBy (CoordSysView p) x = CoordSysView $ translatePairBy p x


-- | Scale an Int pair by changing its length to be `x` times the pair size.
scalePairBy :: Pair BigInt
            -> Number
            -> Pair BigInt
scalePairBy p x = result
  where x' = max zero x
        p'@(Pair l' r') = BigInt.toNumber <$> p
        delta = ((pairSize p' * x') - (pairSize p')) / 2.0
        result = BigInt.fromNumber <$> Pair (l' - delta) (r' + delta)


scaleViewBy :: CoordSysView
            -> Number
            -> CoordSysView
scaleViewBy (CoordSysView p) x = CoordSysView $ scalePairBy p x
