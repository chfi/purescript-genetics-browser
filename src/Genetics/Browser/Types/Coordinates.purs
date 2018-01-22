module Genetics.Browser.Types.Coordinates where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Filterable (class Filterable, filter)
import Data.Foldable (length, sum)
import Data.Int as Int
import Data.Lens (Lens', Traversal', foldMapOf, lens, previewOn, traversed, view)
import Data.Lens (filtered) as Lens
import Data.Lens.Record (prop) as Lens
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Pair (Pair(..))
import Data.Ratio (Ratio, (%))
import Data.Ratio as Ratio
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(Tuple), uncurry)
import Genetics.Browser.Types (Bp, Point)
import Genetics.Browser.View (Pixels)




-- The global coordinate system works by taking the sum of the chromosome sizes;
-- we represent it using a BigInt

-- Consider making it a wrapper on Ratio BigInt instead.
-- Would ensure exactness when moving between global & interval points...
-- ... while that shouldn't be a problem anyway, it would make things feel safer.
newtype BrowserPoint = BPoint BigInt

derive instance eqBrowserPoint :: Eq BrowserPoint
derive instance ordBrowserPoint :: Ord BrowserPoint
derive instance newtypeBrowserPoint :: Newtype BrowserPoint _

derive newtype instance ringBrowserPoint :: Ring BrowserPoint
derive newtype instance semiringBrowserPoint :: Semiring BrowserPoint
derive newtype instance commutativeringBrowserPoint :: CommutativeRing BrowserPoint
derive newtype instance euclideanringBrowserPoint :: EuclideanRing BrowserPoint


-- Subsets of the global coordinate system are defined by some interval,
--

-- data Interval c = Interval c c

type Interval = Pair

type RelPoint = Ratio BigInt
-- data IntervalPoint c = IntervalPoint (Interval c) RelPoint

relPointToNumber :: RelPoint -> Number
relPointToNumber p =
  let d = Ratio.denominator p
      n = Ratio.numerator p
      qt = BigInt.toNumber $ n / d
      -- rm = BigInt.toNumber $ case n `mod` d of
      --                          0 -> 1
      --                          x -> x
      -- TODO be safer by using Number.EPSILON etc.
      -- eps =  2.220446049250313e-16
      -- epsD =  BigInt.fromString "22204460492503130000000000000000"
  in qt -- + (rm / (BigInt.toNumber d))

intervalSize :: forall c.
                Ring c
             => Interval c
             -> c
intervalSize (Pair l r) = r - l

inInterval :: forall c.
               Ord c
            => c
            -> Interval c
            -> Boolean
inInterval p (Pair l r) = l <= p && p <= r


-- TODO test this
intervalsOverlap :: forall c.
                    Ord c
                 => Interval c
                 -> Interval c
                 -> Boolean
intervalsOverlap (Pair l1 r1) (Pair l2 r2) =
  let (Pair l1' r1') = Pair (min l1 l2) (min r1 r2)
      (Pair l2' r2') = Pair (max l1 l2) (max r1 r2)
  in r1' >= l2'

p0 = Pair 0 0
p1 = Pair 0 2
p2 = Pair 2 4
p3 = Pair 3 4
p4 = Pair (-2) (-1)


coveringIntervals :: forall f i c.
                     Ord c
                  => Filterable f
                  => Interval c
                  -> f (Interval c)
                  -> f (Interval c)
coveringIntervals iv = filter (intervalsOverlap iv)


-- type ChrInterval c = { interval :: Interval c, chrSize :: Number }


type CoordInterval i c = { index :: i
                         , interval :: Interval c
                         , chrSize :: Bp }



_Index :: forall i c. Lens' (CoordInterval i c) i
_Index = Lens.prop (SProxy :: SProxy "index")

_Interval :: forall i c. Lens' (CoordInterval i c) (Interval c)
_Interval = Lens.prop (SProxy :: SProxy "interval")

_ChrSize :: forall i c. Lens' (CoordInterval i c) Bp
_ChrSize = Lens.prop (SProxy :: SProxy "chrSize")



type CoordSysRec i c =
  { size :: c
  , padding :: c
  , intervals :: Array (CoordInterval i c)
  }

newtype CoordSys i c =
  CoordSys (CoordSysRec i c)

-- traversed :: forall t a b. Traversable t => Traversal (t a) (t b) a b
_CoordSys :: forall i c. Lens' (CoordSys i c) (CoordSysRec i c)
_CoordSys = lens (\(CoordSys x) -> x) (\(CoordSys _) x -> CoordSys x)

_BrowserIntervals :: forall i c. Lens' (CoordSys i c) (Array (CoordInterval i c))
_BrowserIntervals =   _CoordSys
                  <<< Lens.prop (SProxy :: SProxy "intervals")

_BrowserSize :: forall i c. Lens' (CoordSys i c) c
_BrowserSize =   _CoordSys
             <<< Lens.prop (SProxy :: SProxy "size")

_BrowserPadding :: forall i c. Lens' (CoordSys i c) c
_BrowserPadding =   _CoordSys
                <<< Lens.prop (SProxy :: SProxy "padding")


findIntervalFromGlobal :: forall f i c r.
                          Ord c
                       => c
                       -> Array { interval :: Interval c | r }
                       -> Maybe { interval :: Interval c | r }
findIntervalFromGlobal x = Array.find (inInterval x <<< _.interval)


findBrowserInterval :: forall i c.
                       Ord c
                    => CoordSys i c
                    -> c
                    -> Maybe (CoordInterval i c)
findBrowserInterval cs x = previewOn cs
                            (_BrowserIntervals
                             <<< traversed
                             <<< Lens.filtered
                             (inInterval x <<< view _Interval))

viewIntervals :: forall i c.
                 Ord c
              => CoordSys i c
              -> Interval c
              -> Array (CoordInterval i c)
viewIntervals cs vw = foldMapOf inView pure cs
  where inView = _BrowserIntervals
                 <<< traversed
                 <<< Lens.filtered
                 (intervalsOverlap vw <<< view _Interval)


globalToInterval :: forall c.
                    Newtype c BigInt
                 => Interval c
                 -> c
                 -> RelPoint
globalToInterval v x = globalToInterval' (map unwrap v) (unwrap x)

globalToInterval' :: forall c.
                     Ord c
                  => EuclideanRing c
                  => Interval c
                  -> c
                  -> Ratio c
globalToInterval' (Pair l r) x =
  let n = x - l
      d = max one (r - l)
  in n % d


-- Global <-> Interval are isomorphic for big enough intervals...
intervalToGlobal :: forall c.
                    Newtype c BigInt
                 => Interval c
                 -> RelPoint
                 -> c
intervalToGlobal v x = wrap $ intervalToGlobal' (map unwrap v) x

intervalToGlobal' :: forall c.
                     Ord c
                  => Show c
                  => EuclideanRing c
                  => Interval c
                  -> Ratio c
                  -> c
intervalToGlobal' iv@(Pair l r) p =
  let d1 = max one (r - l)
      d2 = Ratio.denominator p
      a = d1 / d2
      n = a * Ratio.numerator p
  in l + n



offsets :: BigInt -> Array BigInt -> Array (Pair BigInt)
offsets pad xs = ivals
  where os = Array.scanl (\x y -> pad + x + y) zero xs
        ivals = Array.zipWith Pair (zero `Array.cons` os) os


mkCoordSys :: forall i c.
           Array (Tuple i BigInt)
        -> BigInt
        -> CoordSys i BrowserPoint
mkCoordSys chrs pad = CoordSys { size
                               , intervals
                               , padding
                               }
  where (Tuple ids sizes) = Array.unzip chrs
        ivals = (map <<< map) wrap $ offsets pad sizes


        f :: i -> BigInt -> { index :: i, chrSize :: Bp }
        f index s = { index, chrSize: wrap $ BigInt.toNumber s }

        g :: Interval BrowserPoint -> { index :: i, chrSize :: _ } -> _
        g interval { index, chrSize } = { interval, index, chrSize }

        intervals :: Array (CoordInterval i _)
        intervals = Array.zipWith g ivals (uncurry f <$> chrs)

        size' = (sum sizes) + (length sizes * pad)
        size :: BrowserPoint
        size = wrap size'
        padding :: BrowserPoint
        padding = wrap pad


canvasToView :: forall r.
                { width :: Number | r }
             -> Number
             -> RelPoint
canvasToView {width} x = globalToInterval' (Pair zero w') x'
  where (Pair x' w') = BigInt.fromInt <<< Int.round <$> Pair x width


canvasToBrowserPoint :: forall r.
                        { width :: Number | r }
                     -> Interval BrowserPoint
                     -> Number
                     -> BrowserPoint
canvasToBrowserPoint cw v@(Pair vL vR) x =
  intervalToGlobal v $ (canvasToView cw x)


browserPointToCanvas :: forall r.
                        { width :: Number | r }
                     -> Interval BrowserPoint
                     -> BrowserPoint
                     -> Number
browserPointToCanvas screenSize v@(Pair vL vR) p = relPointToNumber pixels'
  where viewSize :: BigInt
        viewSize = unwrap $ intervalSize v
        width' = BigInt.fromInt $ Int.round screenSize.width
        -- Pixels/BrowserPoint
        scale :: Ratio BigInt
        scale = width' % viewSize
        pixels' = (unwrap (p - vL) % one) * scale


intervalToScreen :: forall i c r.
                    { width :: Number | r }
                 -> Interval BrowserPoint
                 -> Interval BrowserPoint
                 -> { width :: Number, offset :: Number }
intervalToScreen screen v interval = {width, offset}
  where (Pair l r) = browserPointToCanvas screen v <$> interval
        width = r - l
        offset = l


shiftIntervalBy :: forall c.
                   Newtype c BigInt
                => Interval c
                -> Ratio BigInt
                -> Interval c
shiftIntervalBy v rat = wrap <$> shiftIntervalBy' (unwrap <$> v) rat

shiftIntervalBy' :: forall c.
                    EuclideanRing c
                 => Interval c
                 -> Ratio c
                 -> Interval c
shiftIntervalBy' v@(Pair l r) rat =
  let diff = ((r - l) * (Ratio.numerator rat)) / (Ratio.denominator rat)
  in Pair (l + diff) (r + diff)


zoomIntervalBy :: forall c.
                  Newtype c BigInt
               => Interval c
               -> Ratio BigInt
               -> Interval c
zoomIntervalBy v r = wrap <$> zoomIntervalBy' (unwrap <$> v) r

zoomIntervalBy' :: forall c.
                   EuclideanRing c
                => Interval c
                -> Ratio c
                -> Interval c
zoomIntervalBy' v@(Pair l r) rat =
  let diff = ((r - l) * (Ratio.numerator rat)) / (Ratio.denominator rat)
  in Pair (l - diff) (r + diff)


newtype Normalized a = Normalized a

derive instance newtypeNormalized :: Newtype (Normalized a) _

normalizePoint :: { width :: Number
                  , height :: Number }
               -> Point
               -> Normalized Point
normalizePoint {width, height} {x, y} = Normalized { x: x', y: y' }
  where x' = x / width
        y' = y / height

normPoint :: Point -> Maybe (Normalized Point)
normPoint p@{x, y}
  | x < 0.0 || x > 1.0 = Nothing
  | y < 0.0 || y > 1.0 = Nothing
  | otherwise = Just $ Normalized p

nPointToFrame :: Pixels -> Pixels -> Normalized Point -> Point
nPointToFrame w h (Normalized p) = {x: p.x * w, y: p.y * h}
