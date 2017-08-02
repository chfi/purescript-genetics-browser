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
import Data.List (List)
import Data.Maybe (Maybe, Maybe(..))
import Data.Record.Unsafe (unsafeGet)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, expand, match, on, prj)
import Type.Prelude (class RowLacks)
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



class ConsHandler (rin :: # Type) (rfun :: # Type) (l :: Symbol) a b (rin2 :: # Type) (rfun2 :: # Type)
      | rin l a -> rin2, rfun l a b -> rfun2

instance consHandler
         :: ( RowLacks l rin2
            , RowLacks l rfun2
            , RowCons l a rin1 rin2
            , RowCons l (a -> b) rfun1 rfun2
            )
         => ConsHandler rin rfun l a b rin2 rfun2

class AppendHandlerImpl (rin :: # Type) (rfun :: # Type) (l :: RowList) a b (rin2 :: # Type) (rfun2 :: # Type)

instance appendHandlerNil
         :: AppendHandlerImpl rin rfun Nil a b rin2 rfun2


instance appendHandlerCons
         :: ConsHandler rin rfun k a b rin2 rfun2
         => AppendHandlerImpl rin rfun (Cons k ty rest) a b rin2 rfun2


class AppendHandler (rin :: # Type) (rfun :: # Type) (l :: # Type) a b (rin2 :: # Type) (rfun2 :: # Type)

instance appendHandlerInstance
         :: ( RowToList l llist
            , AppendHandlerImpl rin rfun llist a b rin2 rfun2
            )
         => AppendHandler rin rfun ls a b rin2 rfun2


class ConcatHandler (rin1 :: # Type) (rfun1 :: # Type)
                    (rin2 :: # Type) (rfun2 :: # Type)
                    (rin3 :: # Type) (rfun3 :: # Type)
      | rin1 rin2 -> rin3, rfun1 rfun2 -> rfun3
      , rin1 rin3 -> rin2, rfun1 rfun3 -> rfun2
      , rin2 rin3 -> rin1, rfun2 rfun3 -> rfun1

class ConcatHandlerImpl (lin1 :: RowList) (lfun1 :: RowList)
                        (lin2 :: RowList) (lfun2 :: RowList)
                        (lin3 :: RowList) (lfun3 :: RowList)

instance concatHandlerNil :: ConcatHandlerImpl Nil Nil Nil Nil Nil Nil



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


forkHandler :: ∀ lt a rin rfun eff.
               Union lt a rin
            => InputHandler rin rfun (Eff (avar :: AVAR | eff) Unit)
            -> BusRW (Variant lt)
            -> Aff ( avar :: AVAR | eff ) (Canceler ( avar :: AVAR | eff ))
forkHandler h bus = forkAff $ forever do
  val <- Bus.read bus
  liftEff $ applyHandler h val


-- 'a' is the input type; the type of data that this handler can parse
-- 'rout' is the row of types that this can produce
-- 'rfun' is the row of types that corresponds to the record with functions
data OutputHandler a (rout :: # Type) (rfun :: # Type) = OutputHandler (Record rfun)


-- appendOutputHandler :: ∀ l a rout1 rout2 rfun1 rfun2.
                    --    RowLacks l rout1
                    -- => RowLacks l rfun1
                    -- => RowCons l (a -> Maybe rout1) rfun1 rfun2
                    -- => RowCons l a rout1 rout2
                    -- => IsSymbol l
                    -- => SProxy l
                    -- -> (a -> Maybe rout1)
                    -- -> OutputHandler rout1 rfun1 a
                    -- -> OutputHandler rout2 rfun2 a
appendOutputHandler :: ∀ l a b rout1 rout2 rfun1 rfun2.
                       RowLacks l rout1
                    => RowLacks l rfun1
                    => RowCons l (a -> Maybe b) rfun1 rfun2
                    => RowCons l b rout1 rout2
                    => IsSymbol l
                    => SProxy l
                    -> (a -> Maybe b)
                    -> OutputHandler a rout1 rfun1
                    -> OutputHandler a rout2 rfun2
appendOutputHandler l f (OutputHandler r) = OutputHandler $ insert l f r


emptyOutputHandler :: ∀ a.
                      OutputHandler a () ()
emptyOutputHandler = OutputHandler {}


runOutputHandler :: ∀ lt a rout rfun b.
                    -- Union lt a rin
                    OutputHandler a rout rfun
                 -> a
                 -> List (Variant rout)
runOutputHandler (OutputHandler h) v = ?help
  -- case coerceV v of
  --   Tuple tag a -> a # unsafeGet tag h
  -- where coerceV :: ∀ c. Variant lt -> Tuple String c
  --       coerceV = unsafeCoerce
