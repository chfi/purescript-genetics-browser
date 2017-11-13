module Genetics.Browser.Types
       ( Point
       , Pos
       ) where

import Genetics.Browser.Units (Bp(..), Chr(..))

type Point = { x :: Number, y :: Number}

type Pos = { chr :: Chr, bp :: Bp }
