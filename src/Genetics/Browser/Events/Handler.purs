module Genetics.Browser.Events.Handler where

import Data.Record
import Prelude
import Type.Row
import Control.Monad.Aff.Bus as Bus
import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, Canceler, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Rec.Class (forever)
import Data.List (List, (:), mapMaybe)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Record.Unsafe (unsafeGet)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, expand, match, on, prj, inj)
import Type.Prelude (class RowLacks)
import Unsafe.Coerce (unsafeCoerce)

-- 'rin' is the row of types that this can handle,
-- 'rfun' is the row of types that corresponds to the record with functions
-- 'b' is the output type; all functions in 'rfun' must have 'b' as domain
data InputHandler (rin :: # Type) (rfun :: # Type) b = InputHandler (Record rfun)



appendInputHandler :: ∀ l a rin1 rin2 b rfun1 rfun2.
                     RowLacks l rin1
                   => RowLacks l rfun1
                   => RowCons l (a -> b) rfun1 rfun2
                   => RowCons l a rin1 rin2
                   => IsSymbol l
                   => SProxy l
                   -> (a -> b)
                   -> InputHandler rin1 rfun1 b
                   -> InputHandler rin2 rfun2 b
appendInputHandler l f (InputHandler r) = InputHandler $ insert l f r



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



emptyInputHandler :: ∀ b.
                InputHandler () () b
emptyInputHandler = InputHandler {}


applyInputHandler :: ∀ lt a rin rfun b.
                     Union lt a rin
                  => InputHandler rin rfun b
                  -> Variant lt
                  -> b
applyInputHandler (InputHandler h) v =
  case coerceV v of
    Tuple tag a -> a # unsafeGet tag h
  where coerceV :: ∀ c. Variant lt -> Tuple String c
        coerceV = unsafeCoerce


forkInputHandler :: ∀ lt a rin rfun eff env.
                    Union lt a rin
                 => InputHandler rin rfun (env -> Eff (avar :: AVAR | eff) Unit)
                 -> env
                 -> BusRW (Variant lt)
                 -> Aff ( avar :: AVAR | eff ) (Canceler ( avar :: AVAR | eff ))
forkInputHandler h env bus = forkAff $ forever do
  val <- Bus.read bus
  liftEff $ applyInputHandler h val $ env


-- 'a' is the input type; the type of data that this handler can parse
-- 'rout' is the row of types that this can produce
data OutputHandler a (rout :: # Type) = OutputHandler (List (a -> Maybe (Variant rout)))

appendOutputHandler :: ∀ l a b r1 r r2.
                       Union r1 r r2
                    => RowLacks l r1
                    => RowCons l b r1 r2
                    => IsSymbol l
                    => SProxy l
                    -> (a -> Maybe b)
                    -> OutputHandler a r1
                    -> OutputHandler a r2
appendOutputHandler l f (OutputHandler h) = OutputHandler $ f' : (map <<< map <<< map) expand h
  where f' :: a -> Maybe (Variant r2)
        f' a = inj l <$> f a


emptyOutputHandler :: ∀ a.
                      OutputHandler a ()
emptyOutputHandler = OutputHandler mempty


applyOutputHandler :: ∀ a rout.
                      OutputHandler a rout
                   -> a
                   -> List (Variant rout)
applyOutputHandler (OutputHandler h) a = mapMaybe (\f -> f a) h
