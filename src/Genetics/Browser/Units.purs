module Genetics.Browser.Units where

import Prelude
import Data.Newtype (class Newtype, unwrap)

newtype Bp = Bp Number
derive instance newtypeBp :: Newtype Bp _
derive newtype instance eqBp :: Eq Bp
derive newtype instance ordBp :: Ord Bp
derive newtype instance fieldBp :: Field Bp
derive newtype instance semiringBp :: Semiring Bp
derive newtype instance ringBp :: Ring Bp

newtype MBp = MBp Number
derive instance newtypeMBp :: Newtype MBp _
derive newtype instance eqMBp :: Eq MBp
derive newtype instance ordMBp :: Ord MBp
derive newtype instance fieldMBp :: Field MBp


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
  mbp (Bp x) = MBp (x * 0.0000001)

instance hCoordMBp :: HCoordinate MBp where
  bp (MBp x) = Bp (x * 1000000.0)
  mbp  = id
