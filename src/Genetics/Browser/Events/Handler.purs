module Genetics.Browser.Events.Handler
       ( InputHandler
       , emptyInputHandler
       , appendInputHandler
       , applyInputHandler
       , forkInputHandler
       , OutputHandler
       , emptyOutputHandler
       , appendOutputHandler
       , applyOutputHandler
       ) where

import Prelude

import Control.Monad.Aff (Aff, Canceler, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Rec.Class (forever)
import Data.List (List, (:), mapMaybe)
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Data.Record (insert)
import Data.Record.Unsafe (unsafeGet)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, expand, inj)
import Type.Row (class RowLacks, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)


-- | An InputHandler is basically a record of functions which can handle some
-- | input in the form of Variants.
-- | `rin` is the row of types that this can handle,
-- | `rfun` is the row of types that corresponds to the record with functions
-- | `out` is the output type; all functions in `rfun` must have `out` as output
data InputHandler (rin :: # Type) (rfun :: # Type) out = InputHandler (Record rfun)


-- | Given a label and function, adds a handler to an existing InputHandler.
-- | The label must not already be handled by the InputHandler.
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

class AppendHandlerImpl (rin :: # Type) (rfun :: # Type) (lin :: RowList) (lfun :: RowList) a b (rin2 :: # Type) (rfun2 :: # Type)

instance appendHandlerNil
         :: AppendHandlerImpl rin rfun Nil Nil a b rin2 rfun2


instance appendHandlerCons
         :: ( AppendHandlerImpl rin rfun lin lfun a b rin2 rfun2
            )
         => AppendHandlerImpl rin rfun (Cons label a lin) (Cons label (a -> b) lfun) a b rin2 rfun2


class AppendHandler (rin :: # Type) (rfun :: # Type) (l :: # Type) a b (rin2 :: # Type) (rfun2 :: # Type)

-- instance appendHandlerInstance
--          :: ( RowToList l llist
--             , AppendHandlerImpl rin rfun lin lfun a b rin2 rfun2
--             )
--          => AppendHandler rin rfun lin lfun a b rin2 rfun2



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


-- | Runs a handler on an input, where the input is a Variant whose row is a subset of the
-- | InputHandler input row.
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


-- | Forks an Aff process with a Handler that performs effects, using some environment.
-- | For example, `env` could be a Biodalliance instance.
forkInputHandler :: ∀ lt a rin rfun eff env.
                    Union lt a rin
                 => InputHandler rin rfun (env -> Eff (avar :: AVAR | eff) Unit)
                 -> env
                 -> BusRW (Variant lt)
                 -> Aff ( avar :: AVAR | eff ) (Canceler ( avar :: AVAR | eff ))
forkInputHandler h env bus = forkAff $ forever do
  val <- Bus.read bus
  liftEff $ applyInputHandler h val $ env


-- | An OutputHandler has a list of parsers for some input, and can be used to
-- | produce heterogenous lists of parsed values
-- | `in` is the input type; the type of data that this handler can parse
-- | `rout` is the row of types that this can produce
data OutputHandler a (rout :: # Type) = OutputHandler (List (a -> Maybe (Variant rout)))


-- | Given a label and parser, adds a handler to an existing OutputHandler.
-- | The label must not already be produced by the OutputHandler.
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


-- | Runs an OutputHandler, producing a list of all successful parses of the input.
applyOutputHandler :: ∀ a rout.
                      OutputHandler a rout
                   -> a
                   -> List (Variant rout)
applyOutputHandler (OutputHandler h) a = mapMaybe (\f -> f a) h
