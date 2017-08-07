module Genetics.Browser.Events.Handler
       ( TrackSink
       , emptyTrackSink
       , appendTrackSink
       , applyTrackSink
       , forkTrackSink
       , TrackSource
       , emptyTrackSource
       , appendTrackSource
       , applyTrackSource
       , class AppendHandlerImpl
       , class AppendHandler
       , concatHandler
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
import Type.Prelude (class RowToList)
import Type.Row (class RowLacks, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)


-- | TrackSink is basically a record of functions which can handle some
-- | input in the form of Variants.
-- | `rin` is the row of types that this can handle,
-- | `rfun` is the row of types that corresponds to the record with functions
-- | `out` is the output type; all functions in `rfun` must have `out` as output
data TrackSink (rin :: # Type) (rfun :: # Type) out = TrackSink (Record rfun)


-- | Given a label and function, adds a handler to an existing TrackSink.
-- | The label must not already be handled by the TrackSink.
appendTrackSink :: ∀ l a rin1 rin2 b rfun1 rfun2.
                      RowLacks l rin1
                   => RowLacks l rfun1
                   => RowCons l (a -> b) rfun1 rfun2
                   => RowCons l a rin1 rin2
                   => IsSymbol l
                   => SProxy l
                   -> (a -> b)
                   -> TrackSink rin1 rfun1 b
                   -> TrackSink rin2 rfun2 b
appendTrackSink l f (TrackSink r) = TrackSink $ insert l f r



class AppendHandlerImpl
      (rin :: # Type) (rfun :: # Type)
      (lin :: RowList) (lfun :: RowList)
      a
      b
      (rin2 :: # Type) (rfun2 :: # Type)

instance appendHandlerNil
         :: AppendHandlerImpl rin rfun Nil Nil a b rin2 rfun2


instance appendHandlerCons
         :: ( AppendHandlerImpl rin rfun lin lfun a b rin2 rfun2
            , RowLacks label rin
            , RowLacks label rfun
            , RowCons label (a -> b) rfun rfun2
            , RowCons label a rin rin2
            , IsSymbol label
            )
         => AppendHandlerImpl rin rfun (Cons label a lin) (Cons label (a -> b) lfun) a b rin2 rfun2


class AppendHandler
      (rin :: # Type) (rfun :: # Type)
      (rin2 :: # Type) (rfun2 :: # Type)
      (rin3 :: # Type) (rfun3 :: # Type)
      out

instance appendHandlerInstance
         :: ( RowToList rin2 lin
            , RowToList rfun2 lfun
            , AppendHandlerImpl rin rfun lin lfun a out rin3 rfun3
            )
         => AppendHandler rin rfun rin2 rfun2 rin3 rfun3 out


foreign import concatHandler :: forall rin1 rfun1 rin2 rfun2 rin3 rfun3 out.
                                AppendHandler rin1 rfun1 rin2 rfun2 rin3 rfun3 out
                             => TrackSink rin1 rfun1 out
                             -> TrackSink rin2 rfun2 out
                             -> TrackSink rin3 rfun3 out
-- concatHandler (TrackSink h1) (TrackSink h2) =

-- instance appendHandlerInstance
--          :: ( RowToList l llist
--             , AppendHandlerImpl rin rfun lin lfun a b rin2 rfun2
--             )
--          => AppendHandler rin rfun lin lfun a b rin2 rfun2






emptyTrackSink :: ∀ b.
                TrackSink () () b
emptyTrackSink = TrackSink {}


-- | Runs a handler on an input, where the input is a Variant whose row is a subset of the
-- | TrackSink input row.
applyTrackSink :: ∀ lt a rin rfun b.
                     Union lt a rin
                  => TrackSink rin rfun b
                  -> Variant lt
                  -> b
applyTrackSink (TrackSink h) v =
  case coerceV v of
    Tuple tag a -> a # unsafeGet tag h
  where coerceV :: ∀ c. Variant lt -> Tuple String c
        coerceV = unsafeCoerce


-- | Forks an Aff process with a Handler that performs effects, using some environment.
-- | For example, `env` could be a Biodalliance instance.
forkTrackSink :: ∀ lt a rin rfun eff env.
                    Union lt a rin
                 => TrackSink rin rfun (env -> Eff (avar :: AVAR | eff) Unit)
                 -> env
                 -> BusRW (Variant lt)
                 -> Aff ( avar :: AVAR | eff ) (Canceler ( avar :: AVAR | eff ))
forkTrackSink h env bus = forkAff $ forever do
  val <- Bus.read bus
  liftEff $ applyTrackSink h val $ env


-- | An TrackSource has a list of parsers for some input, and can be used to
-- | produce heterogenous lists of parsed values
-- | `in` is the input type; the type of data that this handler can parse
-- | `rout` is the row of types that this can produce
data TrackSource a (rout :: # Type) = TrackSource (List (a -> Maybe (Variant rout)))


-- | Given a label and parser, adds a handler to an existing TrackSource.
-- | The label must not already be produced by the TrackSource.
appendTrackSource :: ∀ l a b r1 r r2.
                       Union r1 r r2
                    => RowLacks l r1
                    => RowCons l b r1 r2
                    => IsSymbol l
                    => SProxy l
                    -> (a -> Maybe b)
                    -> TrackSource a r1
                    -> TrackSource a r2
appendTrackSource l f (TrackSource h) = TrackSource $ f' : (map <<< map <<< map) expand h
  where f' :: a -> Maybe (Variant r2)
        f' a = inj l <$> f a


emptyTrackSource :: ∀ a.
                      TrackSource a ()
emptyTrackSource = TrackSource mempty


-- | Runs an TrackSource, producing a list of all successful parses of the input.
applyTrackSource :: ∀ a rout.
                      TrackSource a rout
                   -> a
                   -> List (Variant rout)
applyTrackSource (TrackSource h) a = mapMaybe (\f -> f a) h
