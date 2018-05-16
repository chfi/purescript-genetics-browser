module Test.Units where

import Prelude

import Data.Int (toNumber)
import Genetics.Browser.Types (class HCoordinate, Bp(..), MBp(..), bp, mbp)

isomorphism :: ∀ c. (HCoordinate c) => c -> Boolean
isomorphism x = bp x  - bp (mbp x) < Bp 0.000001 &&
                mbp x - mbp (bp x) < MBp 0.000001
