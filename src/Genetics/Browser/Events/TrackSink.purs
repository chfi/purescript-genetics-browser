module Genetics.Browser.Events.TrackSink where
       -- ( TrackSink
       -- , emptyTrackSink
       -- , mkTrackSink
       -- , appendTrackSinks
       -- , applyTrackSink
       -- , forkTrackSink
       -- ) where

import Prelude

import Control.Monad.Aff (Aff, Canceler, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Rec.Class (forever)
import Control.MonadPlus (guard)
import Data.Argonaut (Json)
import Data.Foldable (foldMap, length)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Genetics.Browser.Events.Types (Event)


type SinkConfig a = { eventName :: String
                    , eventFun :: Json -> a
                    }

newtype TrackSink a = TrackSink (StrMap (Json -> a))

derive instance functorTrackSink :: Functor TrackSink
derive instance newtypeTrackSink :: Newtype (TrackSink a) _

instance semigroupTrackSink :: Semigroup (TrackSink a) where
  append (TrackSink s1) (TrackSink s2) = TrackSink (StrMap.union s1 s2)

instance monoidTrackSink :: Monoid (TrackSink a) where
  mempty = TrackSink StrMap.empty


makeTrackSink :: SinkConfig ~> TrackSink
makeTrackSink sc = TrackSink $ StrMap.singleton sc.eventName sc.eventFun

makeTrackSinks :: ∀ a.
                  List (SinkConfig a)
               -> Maybe (TrackSink a)
makeTrackSinks scs = do
  let sinks = foldMap makeTrackSink scs
  -- basic check to make sure there are no overlapping configs,
  -- since they'd be removed by StrMap.union
  guard $ length scs == (StrMap.size $ unwrap sinks)
  pure $ sinks


runSink :: ∀ a. TrackSink a -> Event -> Maybe a
runSink (TrackSink sink) event = do
  f <- StrMap.lookup event.name sink
  pure $ f event.evData


forkTrackSink :: ∀ env eff.
                 TrackSink (env -> Eff eff Unit)
              -> env
              -> BusRW Event
              -> Aff (avar :: AVAR | eff) (Canceler (avar :: AVAR | eff))
forkTrackSink sink env bus = forkAff $ forever do
  event <- Bus.read bus
  let effect :: Maybe (env -> Eff (avar :: AVAR | eff) Unit)
      effect = (map unsafeCoerceEff) <$> runSink sink event

  case effect of
    Nothing -> pure unit
    Just f  -> liftEff $ f env
