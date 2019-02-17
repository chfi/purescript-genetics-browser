module Genetics.Browser.Types where

import Prelude

import Foreign.Class (class Decode, class Encode)
import Data.Lens (Getter', iso, to)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Types (Iso')
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number.Format as Num
import Data.String as String
import Math as Math

type Point = { x :: Number, y :: Number}

type Chr = { chrId :: ChrId, size :: Bp }


-- | Newtype wrapper for a basepair location
newtype Bp = Bp Number
derive instance newtypeBp :: Newtype Bp _
derive newtype instance eqBp :: Eq Bp
derive newtype instance ordBp :: Ord Bp
derive newtype instance semiringBp :: Semiring Bp
derive newtype instance ringBp :: Ring Bp
derive newtype instance divisionRingBp :: DivisionRing Bp
derive newtype instance euclideanRingBp :: EuclideanRing Bp
derive newtype instance commutativeRingBp :: CommutativeRing Bp
derive newtype instance encodeBp :: Encode Bp
derive newtype instance decodeBp :: Decode Bp

instance showBp :: Show Bp where
  show (Bp n) = Num.toStringWith (Num.fixed 0) n <> " Bp"


_Bp :: Iso' Bp Number
_Bp = _Newtype

-- | Newtype wrapper for a chromosome identifier
newtype ChrId = ChrId String
derive instance newtypeChrId :: Newtype ChrId _

chrId :: String -> ChrId
chrId str =
  let str' = String.toLower str
  in case String.stripPrefix (wrap "chr") str' of
       Nothing  -> wrap $ "chr" <> str'
       Just chr -> wrap chr

validChrId :: ChrId -> ChrId
validChrId chr =
  let chr' = String.toLower $ unwrap chr
  in case String.stripPrefix (wrap "chr") chr' of
       Nothing -> wrap $ "chr" <> chr'
       Just c  -> wrap $ c

instance eqChrId :: Eq ChrId where
  eq chrA chrB =
    let (ChrId a) = validChrId chrA
        (ChrId b) = validChrId chrB
    in a == b

-- TODO this might not even be what we want
instance ordChrId :: Ord ChrId where
  compare chrA chrB =
    let (ChrId a) = validChrId chrA
        (ChrId b) = validChrId chrB
    in a `compare` b

derive newtype instance encodeChrId :: Encode ChrId
derive newtype instance decodeChrId :: Decode ChrId

instance showChrId :: Show ChrId where
  show (ChrId i) = i

_ChrId :: Iso' ChrId String
_ChrId = _Newtype


newtype NegLog10 = NegLog10 Number

derive instance newtypeNegLog10 :: Newtype NegLog10 _


_NegLog10 :: Iso' Number NegLog10
_NegLog10 = iso to from
  where to   p = wrap $ (-((Math.log p) / Math.ln10))
        from (NegLog10 p) = 10.0 `Math.pow` (-p)


_prec :: Int -> Getter' Number String
_prec i = to $ Num.toStringWith (Num.precision i)

_fixed :: Int -> Getter' Number String
_fixed i = to $ Num.toStringWith (Num.fixed i)

_exp :: Int -> Getter' Number String
_exp i = to $ Num.toStringWith (Num.exponential i)
