module Genetics.Browser.Events.TrackSink where
       -- ( TrackSink
       -- , emptyTrackSink
       -- , mkTrackSink
       -- , appendTrackSinks
       -- , applyTrackSink
       -- , forkTrackSink
       -- ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, Canceler, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (throwError)
import Control.Monad.Rec.Class (forever)
import Control.MonadPlus (guard)
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.Foldable (any, foldMap, foldr, length)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Genetics.Browser.Events.Types (Event)


type SinkConfig a = { eventName :: String
                    , eventFun :: Json -> a
                    }

newtype TrackSink a = TrackSink (StrMap (Json -> a))
-- TODO TrackSink i o = TrackSink (StrMap (i -> o))
-- and we get a profunctor which might be nice; could do the same on the SinkConfig

derive instance functorTrackSink :: Functor TrackSink
derive instance newtypeTrackSink :: Newtype (TrackSink a) _

instance semigroupTrackSink :: Semigroup (TrackSink a) where
  append (TrackSink s1) (TrackSink s2) = TrackSink (StrMap.union s1 s2)

instance monoidTrackSink :: Monoid (TrackSink a) where
  mempty = TrackSink StrMap.empty


makeTrackSink :: SinkConfig ~> TrackSink
makeTrackSink sc = TrackSink $ StrMap.singleton sc.eventName sc.eventFun

makeTrackSinks :: ∀ a.
                  Array (SinkConfig a)
               -> Either String (TrackSink a)
makeTrackSinks scs = do
  let count :: StrMap Int
      count = StrMap.fromFoldableWith (+) $ map (\c -> Tuple c.eventName 1) scs
      overlapping = StrMap.filter (_ > 1) count

  when (not StrMap.isEmpty overlapping)
    let error = foldMap (append "\n" <<< show) $ StrMap.keys overlapping
    in throwError $ "Overlapping tracksinks!\n" <> error

  pure $ foldMap makeTrackSink scs


runTrackSink :: ∀ a. TrackSink a -> Event -> Maybe a
runTrackSink (TrackSink sink) event = do
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
      effect = (map unsafeCoerceEff) <$> runTrackSink sink event

  case effect of
    Nothing -> pure unit
    Just f  -> liftEff $ f env
