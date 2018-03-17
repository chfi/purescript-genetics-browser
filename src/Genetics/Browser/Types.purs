module Genetics.Browser.Types where

import Prelude
import Data.Foreign.Class (class Decode, class Encode)
import Data.Lens (iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Types (Iso')
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as String

type Point = { x :: Number, y :: Number}

type Pos = { chrId :: ChrId, bp :: Bp }

type Chr = { chrId :: ChrId, size :: Bp }


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
