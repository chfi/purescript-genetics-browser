module Genetics.Browser.Track
       ( class TrackRecord
       , buildTrack
       , makeTrack
       ) where

import Prelude

import Data.Pair (Pair)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Genetics.Browser.Canvas (TrackContainer)
import Genetics.Browser.Coordinates (CoordSysView)
import Genetics.Browser.Types (Point)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (delete, get)
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy(..), SProxy(..), from)


type TrackInterface a =
  { render   :: Pair Number -> CoordSysView -> Aff Unit
  , hotspots :: Effect (Number -> Point -> Array a) }

class TrackRecord
      (trackList :: RowList)
      (trackRow :: # Type)
      a
      | trackList -> trackRow
      , trackList -> a where
  buildTrack :: RLProxy trackList
             -> Record trackRow
             -> TrackContainer
             -> Aff { render   :: Pair Number -> CoordSysView -> Aff Unit
                    , hotspots :: Effect (Number -> Point -> Array a) }


instance trackRecordRender ::
  ( IsSymbol  name
  , Row.Lacks name trackRow'
  , Row.Cons  name (Pair Number -> CoordSysView -> Effect Unit) trackRow' trackRow
  , TrackRecord trackTail trackRow' a
  ) => TrackRecord (Cons name (Pair Number -> CoordSysView -> Effect Unit) trackTail) trackRow a where
  buildTrack _ r tcont = do

    let name = SProxy :: SProxy name

    rest <- buildTrack (RLProxy :: RLProxy trackTail)
                       (delete name r) tcont

    let rLayer p c = liftEffect $ (get name r) p c

    pure { render:   rLayer <> rest.render
         , hotspots: rest.hotspots }


instance trackRecordUI ::
  ( IsSymbol  name
  , Row.Lacks name trackRow'
  , Row.Cons  name (Effect Unit) trackRow' trackRow
  , TrackRecord trackTail trackRow' a
  ) => TrackRecord (Cons name (Effect Unit) trackTail) trackRow a where
  buildTrack _ r tcont = do

    let name = SProxy :: SProxy name

    rest <- buildTrack (RLProxy :: RLProxy trackTail)
                       (delete name r) tcont

    let rLayer _ _ = liftEffect $ (get name r)

    pure { render:   rLayer <> rest.render
         , hotspots: rest.hotspots }

instance trackRecordHotspots ::
  ( IsSymbol  name
  , Row.Lacks name trackRow'
  , Row.Cons  name (Effect (Number -> pt -> Array a)) trackRow' trackRow
  , TypeEquals pt Point
  , TrackRecord trackTail trackRow' a
  ) => TrackRecord (Cons name (Effect (Number -> pt -> Array a)) trackTail) trackRow a where
  buildTrack _ r tcont = do

    let name = SProxy :: SProxy name

    rest <- buildTrack (RLProxy :: RLProxy trackTail)
                       (delete name r) tcont

    let hotspots = ado
          h  <- get name r
          h' <- rest.hotspots
          in \rad p -> h rad (from p) <> h' rad p

    pure { render: rest.render, hotspots }


instance trackRecordNil ::
  ( TypeEquals (Record trackRow) {}
  ) => TrackRecord Nil trackRow a where
  buildTrack _ _ _ = mempty


makeTrack :: ∀ row list a.
             RowToList row list
          => TrackRecord list row a
          => Record row
          -> TrackContainer
          -> Aff (TrackInterface a)
makeTrack r c = buildTrack (RLProxy :: RLProxy list) r c
