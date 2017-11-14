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


-- TODO This module is probably not the best place for these functions
chrsInRange :: forall aff a r.
               Array Chr
            -> Range r
            -> Maybe (Array Chr)
chrsInRange chrs { lHand, rHand } = do
  lI <- Array.findIndex (\x -> x.chrId == lHand.chrId) chrs
  rI <- Array.findIndex (\x -> x.chrId == rHand.chrId) chrs
  traverse (index chrs) (lI .. rI)


getRangeSize :: forall aff a r.
                Array Chr
             -> Range r
             -> Maybe Bp
getRangeSize chrs r@{ lHand, rHand } = do
  chrs' <- chrsInRange chrs r
  {head, tail} <- Array.uncons chrs'
  {init, last} <- Array.unsnoc tail

  let l = head.size - lHand.bp
      mid = alaF Additive foldMap (_.size) init
      r = rHand.bp
  pure $ l + mid + r
