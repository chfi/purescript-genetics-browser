module Genetics.Browser.Events.Handler where

import Data.Record
import Prelude
import Type.Row

import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, Canceler, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Data.Record.Unsafe (unsafeGet)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, expand, match, on, prj)
import Unsafe.Coerce (unsafeCoerce)

-- 'rin' is the row of types that this can handle,
-- 'rfun' is the row of types that corresponds to the record with functions
-- 'b' is the output type; all functions in 'rfun' must have 'b' as domain
data InputHandler (rin :: # Type) (rfun :: # Type) b = InputHandler (Record rfun)



appendHandler :: ∀ l a rin1 rin2 b rfun1 rfun2.
                 RowLacks l rin1
              => RowLacks l rfun1
              => RowCons l (a -> b) rfun1 rfun2
              => RowCons l a rin1 rin2
              => IsSymbol l
              => SProxy l
              -> (a -> b)
              -> InputHandler rin1 rfun1 b
              -> InputHandler rin2 rfun2 b
appendHandler l f (InputHandler r) = InputHandler $ insert l f r


emptyHandler :: ∀ b.
                InputHandler () () b
emptyHandler = InputHandler {}


applyHandler :: ∀ lt a rin rfun b.
                Union lt a rin
             => InputHandler rin rfun b
             -> Variant lt
             -> b
applyHandler (InputHandler h) v =
  case coerceV v of
    Tuple tag a -> a # unsafeGet tag h
  where coerceV :: ∀ c. Variant lt -> Tuple String c
        coerceV = unsafeCoerce


forkHandler :: ∀ lt a rin rfun.
               Union lt a rin
            => InputHandler rin rfun (Eff _ Unit)
            -> BusRW (Variant lt)
            -> _
forkHandler h bus = forkAff $ forever do
  val <- Bus.read bus
  liftEff $ applyHandler h val
