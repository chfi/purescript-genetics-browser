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


data ScrHCoord = ScrHCoord { offset :: Bp, scale :: Bp } Number
derive instance eqScrHCoord :: Eq ScrHCoord
derive instance ordScrHCoord :: Ord ScrHCoord


class Field a <= HCoordinate a where
  bp :: a -> Bp
  mbp :: a -> MBp
    -- horizontal offset and scale
  -- toScreen :: ∀ c. HCoordinate c => c -> c -> a -> ScrHCoord

toScreen :: ∀ c. HCoordinate c => c -> c -> c -> ScrHCoord
toScreen offset scale x = ScrHCoord { offset: offset'
                                    , scale: scale'
                                    } $ unwrap ((x' - offset') * scale')
  where offset' = bp offset
        scale' = bp scale
        x' = bp x

instance hCoordBp :: HCoordinate Bp where
  bp = id
  mbp (Bp x) = MBp (x * 0.0000001)


instance hCoordMBp :: HCoordinate MBp where
  bp (MBp x) = Bp (x * 0.0000001)
  mbp  = id
  -- toScreen offset scale (MBp x) = ScrHCoord { offset, scale } $ (x - offset) * scale
