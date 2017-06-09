module Genetics.Browser.Units where

-- TODO: feels like there are better ways of dealing with this (units/dimensions);
-- need to look for libraries

import Prelude
import Data.Foreign.Class (class Decode, class Encode)
import Data.Lens (iso, lens, prism, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Types (Iso', Lens', Prism')
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Test.QuickCheck (class Arbitrary)

newtype Bp = Bp Number
derive instance newtypeBp :: Newtype Bp _
derive newtype instance eqBp :: Eq Bp
derive newtype instance ordBp :: Ord Bp
derive newtype instance fieldBp :: Field Bp
derive newtype instance semiringBp :: Semiring Bp
derive newtype instance ringBp :: Ring Bp
derive newtype instance arbitraryBp :: Arbitrary Bp
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

newtype MBp = MBp Number
derive instance newtypeMBp :: Newtype MBp _
derive newtype instance eqMBp :: Eq MBp
derive newtype instance ordMBp :: Ord MBp
derive newtype instance fieldMBp :: Field MBp
derive newtype instance semiringMBp :: Semiring MBp
derive newtype instance ringMBp :: Ring MBp
derive newtype instance arbitraryMBp :: Arbitrary MBp
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


newtype Chr = Chr String
derive instance newtypeChr :: Newtype Chr _
derive newtype instance eqChr :: Eq Chr
derive newtype instance encodeChr :: Encode Chr
derive newtype instance decodeChr :: Decode Chr

_Chr :: Iso' Chr String
_Chr = _Newtype


-- prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> Prism' s a
-- _CyChr :: Prism' String Chr
-- _CyChr = prism' print parse
--   where print :: Chr -> String
--         print (Chr chr) = "Chr" <> show chr
--         parse :: String -> Maybe Chr
--         parse str =
