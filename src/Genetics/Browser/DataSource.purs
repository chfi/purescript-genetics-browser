module Genetics.Browser.DataSource where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Array (index, (..))
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Data.Traversable (traverse)
import Genetics.Browser.Types (Bp(..), Chr, Pos, Range)

type DataSource aff a = { fetch :: Pos -> Pos -> Aff aff (Array a)
                        , chrs :: Array Chr
                        }
