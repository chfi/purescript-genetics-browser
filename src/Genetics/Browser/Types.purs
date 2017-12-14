module Genetics.Browser.Types where
       -- ( Point
       -- , Pos
       -- , Range
       -- , Bp(..)
       -- , MBp(..)
       -- , class HCoordinate
       -- , bp
       -- , mbp
       -- , _Bp
       -- , _MBp
       -- , _BpMBp
       -- , ChrId(..)
       -- , _ChrId
       -- , toScreen
       -- , BpPerPixel(..)
       -- , bpToPixels
       -- , pixelsToBp
       -- , Chr
       -- , ChrInterval
       -- , BrowserPoint(..)
       -- , LocalPoint(..)
       -- , IntervalPoint
       -- , Interval(..)
       -- , CoordSys(..)
       -- , mkCoordSys
       -- , findInterval
       -- , localToGlobal
       -- , globalToLocal
       -- , canvasToView
       -- , globalToFrame
       -- , intervalToChr
       -- , frameToChr
       -- , intervalSize
       -- , canvasToBrowserOffset
       -- ) where

import Prelude

import Control.Alternative (empty)
import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (length, sum)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Int as Int
import Data.Lens (APrism', Iso', Lens', Prism', iso, lens, prism', takeBoth, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Types (Iso')
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Choice (fanin)
import Data.Profunctor.Strong (fanout, splitStrong)
import Data.Ratio (Ratio, (%))
import Data.Ratio as Ratio
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Unsafe.Coerce (unsafeCoerce)
-- import Genetics.Browser.Units (Bp(..), ChrId(..))

type Point = { x :: Number, y :: Number}

type Pos = { chrId :: ChrId, bp :: Bp }

type Chr = { chrId :: ChrId, size :: Bp }

type Range r = { lHand :: Pos, rHand :: Pos  | r}



-- | Newtype wrapper for a basepair location
newtype Bp = Bp Number
derive instance newtypeBp :: Newtype Bp _
derive newtype instance eqBp :: Eq Bp
derive newtype instance ordBp :: Ord Bp
derive newtype instance fieldBp :: Field Bp
derive newtype instance euclideanRingBp :: EuclideanRing Bp
derive newtype instance commutativeRingBp :: CommutativeRing Bp
derive newtype instance semiringBp :: Semiring Bp
derive newtype instance ringBp :: Ring Bp
derive newtype instance encodeBp :: Encode Bp
derive newtype instance decodeBp :: Decode Bp

instance showBp :: Show Bp where
  show (Bp n) = show n <> "Bp"


_Bp :: Iso' Bp Number
_Bp = _Newtype

_MBp :: Iso' MBp Number
_MBp = _Newtype

-- probably not actually an isomorphism due to floating point inaccuracies
_BpMBp :: Iso' Bp MBp
_BpMBp = iso mbp bp

-- | Newtype wrapper for a megabasepair location
newtype MBp = MBp Number
derive instance newtypeMBp :: Newtype MBp _
derive newtype instance eqMBp :: Eq MBp
derive newtype instance ordMBp :: Ord MBp
derive newtype instance fieldMBp :: Field MBp
derive newtype instance euclideanRingMBp :: EuclideanRing MBp
derive newtype instance commutativeRingMBp :: CommutativeRing MBp
derive newtype instance semiringMBp :: Semiring MBp
derive newtype instance ringMBp :: Ring MBp
derive newtype instance encodeMBp :: Encode MBp
derive newtype instance decodeMBp :: Decode MBp


instance showMBp :: Show MBp where
  show (MBp n) = show n <> "MBp"

class Field a <= HCoordinate a where
  bp :: a -> Bp
  mbp :: a -> MBp

toScreen :: ∀ c. HCoordinate c => c -> c -> c -> Number
toScreen offset scale x = unwrap ((x' - offset') * scale')
  where offset' = bp offset
        scale' = bp scale
        x' = bp x

instance hCoordBp :: HCoordinate Bp where
  bp = id
  mbp (Bp x) = MBp (x * 0.000001)

instance hCoordMBp :: HCoordinate MBp where
  bp (MBp x) = Bp (x * 1000000.0)
  mbp  = id

-- | Newtype wrapper for a chromosome identifier
newtype ChrId = ChrId String
derive instance newtypeChrId :: Newtype ChrId _
derive newtype instance eqChrId :: Eq ChrId
derive newtype instance ordChrId :: Ord ChrId
derive newtype instance encodeChrId :: Encode ChrId
derive newtype instance decodeChrId :: Decode ChrId
derive newtype instance showChrId :: Show ChrId

_ChrId :: Iso' ChrId String
_ChrId = _Newtype



-- TODO be *really cool* and represent it as a ratio of bp and pixels
-- i.e. just a tuple!
-- also it doesn't mean anything on its own;
-- or, rather, it's implicit that it only concerns a single chr at origin

newtype BpPerPixel = BpPerPixel Number
derive instance newtypeBpPerPixel :: Newtype BpPerPixel _

bpToPixels :: BpPerPixel -> Bp -> Number
bpToPixels (BpPerPixel s) (Bp p) = p / s

pixelsToBp :: BpPerPixel -> Number -> Bp
pixelsToBp (BpPerPixel s) p = Bp $ p * s


-- newtype BpScale = BpScale (Ratio BigInt BigInt)


-- Describes a number of `a`s per `b` -- `b` is a phantom type;
-- we always assume `b` is 1 in whatever unit it represents.
data UnitRatio a b = UnitRatio a

instance eqUnitRatio :: (Eq a) => Eq (UnitRatio a b) where
  eq (UnitRatio a) (UnitRatio b) = a == b

instance ordUnitRatio :: (Ord a) => Ord (UnitRatio a b) where
  compare (UnitRatio a) (UnitRatio b) = compare a b

type BpPerPixel' = UnitRatio Number Bp



-- pretty much all my problems appear to be solved by this type. nice
-- data Local f c = Local c c (Ratio c)
-- data BrowserWide c = BrowserWide

-- toGlobal :: forall f c.
--             Ord c
--          => EuclideanRing c
--          => Local f c
--          -> Maybe c
-- toGlobal (Local l r p) =
--   let p' = p * ((r - l) % one)
--   in if Ratio.denominator p' == one
--         then pure $ Ratio.numerator p'
--         else Nothing

-- toLocal :: forall f c.
--            Ord c
--         => EuclideanRing c
--         => c
--         -> Tuple c c
--         -> Local f c
-- toLocal c (Tuple l r) = Local l r $ (c - l) % (r - l)




-- The global coordinate system works by taking the sum of the chromosome sizes;
-- we represent it using a BigInt
newtype BrowserPoint = BPoint BigInt

derive instance eqBrowserPoint :: Eq BrowserPoint
derive instance ordBrowserPoint :: Ord BrowserPoint
derive instance newtypeBrowserPoint :: Newtype BrowserPoint _



-- Subsets of the global coordinate system are defined by some interval,
--

data Interval c = Interval c c

inInterval :: forall c.
              Ord c
           => c
           -> Interval c
           -> Boolean
inInterval p (Interval l r) = l <= p && p <= r

_iL :: forall c. Lens' (Interval c) c
_iL = lens (\(Interval l _) -> l) (\(Interval _ r) l -> Interval l r)


_iR :: forall c. Lens' (Interval c) c
_iR = lens (\(Interval _ r) -> r) (\(Interval l _) r -> Interval l r)

-- clunky but works for now
data LocalPoint c = Local (Interval c) (Ratio BigInt)

_lIv :: forall c. Lens' (LocalPoint c) (Interval c)
_lIv = lens (\(Local i _) -> i) (\(Local _ p) i -> Local i p)

_lP :: forall c. Lens' (LocalPoint c) (Ratio BigInt)
_lP = lens (\(Local _ p) -> p) (\(Local i _) p -> Local i p)


-- TODO This is dumb and bad, precision etc.
ratioToNumber :: Ratio BigInt
              -> Number
ratioToNumber r =
  let n = (Ratio.numerator r)
      d = (Ratio.denominator r)
  in (BigInt.toNumber n) / (BigInt.toNumber d)


-- for now we just assume that the given point is in the interval...
globalToLocal :: forall a c.
                 Newtype c BigInt
              => Interval c
              -> c
              -> LocalPoint c
globalToLocal iv@(Interval l r) p =
  let l' = unwrap l
      r' = unwrap r
      p' = ((unwrap p) - l') % (r' - l')
  in Local iv p'


localToGlobal :: forall c.
                 Newtype c BigInt
              => LocalPoint c
              -> c
localToGlobal (Local (Interval l r) p) =
  let r' = (unwrap r) % one
      l' = (unwrap l) % one
      p' = (p * (r' - l')) + l'
  in wrap $ (Ratio.numerator p') / (Ratio.denominator p')


intervalSize :: forall c a.
                Newtype c a
             => Ring a
             => Interval c
             -> a
intervalSize (Interval l r) = (unwrap r) - (unwrap l)


-- | Transform a point in some interval A to the same point in interval B
seenFrom :: forall a b c.
            Newtype c BigInt
         => Newtype b BigInt
         => LocalPoint c
         -> Interval b
         -> LocalPoint b
seenFrom p i =
  let p' = unwrap $ localToGlobal p
  in globalToLocal i (wrap p')


intervalSeenFrom :: forall a b c.
                    Newtype c BigInt
                 => Newtype b BigInt
                 => Interval c
                 -> Interval b
                 -> Interval (Ratio BigInt)
intervalSeenFrom (Interval l r) i2 =
  let pl = globalToLocal i2 (wrap $ unwrap l)
      pr = globalToLocal i2 (wrap $ unwrap r)
  in Interval (pl^._lP) (pr^._lP)


type IntervalPoint = LocalPoint BrowserPoint

type ChrInterval c = { interval :: Interval c, chrSize :: Bp }

newtype CoordSys i c =
  CoordSys { size :: c
           , padding :: c
           -- , intervals :: Array (Tuple i (Interval c))
           , intervals :: Array (Tuple i (ChrInterval c))
           }


-- TODO need to make sure the padding is correctly applied/removed
mkCoordSys :: forall i c.
              Array (Tuple i BigInt)
           -> BigInt
           -> CoordSys i BrowserPoint
mkCoordSys chrs padding = CoordSys { size, padding: wrap padding, intervals }
  where (Tuple ids sizes) = Array.unzip chrs
        os = map BPoint $ Array.scanl (\x y -> padding + x + y) zero sizes
        ivals = Array.zipWith Interval (wrap zero `Array.cons` os) os
        intervals = Array.zip ids $
                    Array.zipWith (\i s -> {interval: i, chrSize:(Bp $ BigInt.toNumber s)}) ivals sizes
        size = wrap $ (sum sizes) + (length sizes * padding)


findInterval :: forall i c.
                Ord c
             => CoordSys i c
             -> c
             -> Maybe (Tuple i (ChrInterval c))
findInterval (CoordSys s) p =
  Array.find (\(Tuple i iv) -> p `inInterval` iv.interval) s.intervals


intervals :: forall i c.
             Ord i
          => CoordSys i c
          -> i
          -> i
          -> Maybe (Array (Tuple i (ChrInterval c)))
intervals (CoordSys s) l r = do
  let l' = min l r
      r' = max l r
  lIndex <- Array.findIndex ((==) l' <<< fst) s.intervals
  rIndex <- Array.findIndex ((==) r' <<< fst) s.intervals
  pure $ Array.slice lIndex rIndex s.intervals


viewIntervals :: forall i c.
                 Ord i
              => Ord c
              => CoordSys i c
              -> Interval c
              -> Maybe (Array (Tuple i (ChrInterval c)))
viewIntervals cs (Interval l r) = do
  lIv <- fst <$> findInterval cs l
  rIv <- fst <$> findInterval cs r

  intervals cs lIv rIv


canvasToView :: forall r.
                { width :: Number | r }
             -> Number
             -> Ratio BigInt
canvasToView {width} x = x' % w'
  where w' = BigInt.fromInt $ Int.round width
        x' = BigInt.fromInt $ Int.round x


canvasToBrowserOffset :: forall r.
                         { width :: Number | r }
                      -> BigInt
                      -> Number
                      -> BrowserPoint
canvasToBrowserOffset w vw x =
  let x' = canvasToView w x
      p' = x' * (vw % one)
  in wrap $ (Ratio.numerator p') / (Ratio.denominator p')


globalToFrame :: forall i c.
                 Eq i
              => CoordSys i BrowserPoint
              -> BrowserPoint
              -> Maybe (Tuple i IntervalPoint)
globalToFrame cs bp@(BPoint p) = do
  (Tuple i iv) <- findInterval cs bp
  pure $ Tuple i $ globalToLocal iv.interval bp


intervalToChr :: BrowserPoint
              -> IntervalPoint
              -> Bp
              -> Maybe Bp
intervalToChr padding (Local (Interval l r) p) chrSize = do
  let padR = (unwrap padding) % (unwrap r - unwrap l)
  -- guard $ p >= padR && p <= one - padR
  let p' = p - padR
      bp = Bp $ (BigInt.toNumber $ Ratio.numerator p') / (BigInt.toNumber $ Ratio.denominator p')

  -- TODO it's dumb that Bp is still a Number newtype
  pure $ bp * chrSize


frameToChr :: forall i c.
              Eq i
           => CoordSys i BrowserPoint
           -> Tuple i IntervalPoint
           -> Maybe Bp
frameToChr (CoordSys s) (Tuple i lp) = do
  iv <- snd <$> Array.find ((==) i <<< fst) s.intervals
  intervalToChr s.padding lp iv.chrSize


intervalToScreen :: BrowserPoint
                 -> Number
                 -> Interval BrowserPoint
                 -> Interval BrowserPoint
                 -> { width :: Number, offset :: Number }
intervalToScreen pad screenWidth reference other = { width, offset }
  where screen :: BigInt
        screen = BigInt.fromInt $ Int.round screenWidth
        padL :: BrowserPoint
        padL = wrap $ (unwrap $ other^._iL) + (unwrap pad)
        padR :: BrowserPoint
        padR = wrap $ (unwrap $ other^._iR) - (unwrap pad)
        -- 1. calculate LHS and RHS of other wrt. reference
        (Interval l r) = (Interval padL padR) `intervalSeenFrom` reference
        l' = ratioToNumber $ l * (screen % one)
        r' = ratioToNumber $ r * (screen % one)
        -- 2. width is obvious
        width :: Number
        width = r' - l'
        -- 3. offset is less so
        offset :: Number
        offset = l'



shiftIntervalBy :: forall c.
                   Newtype c BigInt
                => Interval c
                -> Ratio BigInt
                -> Interval c
shiftIntervalBy (Interval l r) rat =
  let l' = unwrap l
      r' = unwrap r
      diff = ((r' - l') * (Ratio.numerator rat)) / (Ratio.denominator rat)
  in Interval (wrap $ l' - diff) (wrap $ r' - diff)
