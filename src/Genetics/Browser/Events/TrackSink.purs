module Genetics.Browser.Events.TrackSink
       ( TrackSink
       , emptyTrackSink
       , mkTrackSink
       , appendTrackSinks
       , applyTrackSink
       , forkTrackSink
       ) where

import Prelude

import Control.Monad.Aff (Aff, Canceler, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Rec.Class (forever)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Genetics.Browser.Events.Types (Event)

newtype TrackSink env eff = TrackSink (StrMap (Json -> env -> Maybe (Eff eff Unit)))

emptyTrackSink :: ∀ env eff. TrackSink env eff
emptyTrackSink = TrackSink StrMap.empty


mkTrackSink :: ∀ env eff.
               String
            -> (Json -> env -> Maybe (Eff eff Unit))
            -> TrackSink env eff
mkTrackSink l f = TrackSink $ StrMap.singleton l f


-- TODO: StrMap.union might not be the best behavior;
-- indeed we might want this to return Maybe TrackSink
appendTrackSinks :: ∀ env eff.
                    TrackSink env eff
                 -> TrackSink env eff
                 -> TrackSink env eff
appendTrackSinks (TrackSink s1) (TrackSink s2) =
  TrackSink $ StrMap.union s1 s2


applyTrackSink :: ∀ env eff.
                  TrackSink env eff
               -> env
               -> Event
               -> Eff eff Unit
applyTrackSink (TrackSink hs) env ev = case StrMap.lookup ev.eventType hs of
  Nothing -> pure unit
  Just h  -> case h ev.event env of
    Nothing -> pure unit
    Just f  -> f


forkTrackSink :: ∀ env eff.
                 TrackSink env eff
              -> env
              -> BusRW Event
              -> Aff (avar :: AVAR | eff) (Canceler (avar :: AVAR | eff))
forkTrackSink hs env bus = forkAff $ forever do
  val <- Bus.read bus
  liftEff $ unsafeCoerceEff $ applyTrackSink hs env val
