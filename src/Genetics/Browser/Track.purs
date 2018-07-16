module Genetics.Browser.Track
       ( class TrackRecord
       , buildTrack
       , makeTrack
       , TrackConfigRecord
       , class TrackConfig
       , makeContainersImpl
       , makeContainers
       ) where

import Prelude

import Data.Pair (Pair)
import Effect (Effect)
import Effect.Aff (Aff)
import Genetics.Browser.Canvas (TrackContainer, trackContainer)
import Genetics.Browser.Coordinates (CoordSysView)
import Genetics.Browser.Layer (TrackPadding)
import Genetics.Browser.Types (Point)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (delete, get, insert)
import Type.Data.Symbol as Symbol
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy(..), SProxy(..), from)




type TrackInterface a =
  { render   :: Pair Number -> CoordSysView -> Aff Unit
  , hotspots :: Aff (Number -> Point -> Array a) }

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
                    , hotspots :: Aff (Number -> Point -> Array a) }


instance trackRecordRender ::
  ( IsSymbol  name
  , Row.Lacks name trackRow'
  , Row.Cons  name (Pair Number -> CoordSysView -> Aff Unit) trackRow' trackRow
  , TrackRecord trackTail trackRow' a
  ) => TrackRecord (Cons name (Pair Number -> CoordSysView -> Aff Unit) trackTail) trackRow a where
  buildTrack _ r tcont = do

    let name = SProxy :: SProxy name

    rest <- buildTrack (RLProxy :: RLProxy trackTail)
                       (delete name r) tcont

    let rLayer p c = (get name r) p c

    pure { render:   rLayer <> rest.render
         , hotspots: rest.hotspots }


instance trackRecordUI ::
  ( IsSymbol  name
  , Row.Lacks name trackRow'
  , Row.Cons  name (Aff Unit) trackRow' trackRow
  , TrackRecord trackTail trackRow' a
  ) => TrackRecord (Cons name (Aff Unit) trackTail) trackRow a where
  buildTrack _ r tcont = do

    let name = SProxy :: SProxy name

    rest <- buildTrack (RLProxy :: RLProxy trackTail)
                       (delete name r) tcont

    let rLayer _ _ = (get name r)

    pure { render:   rLayer <> rest.render
         , hotspots: rest.hotspots }

instance trackRecordHotspots ::
  ( IsSymbol  name
  , Row.Lacks name trackRow'
  , Row.Cons  name (Aff (Number -> pt -> Array a)) trackRow' trackRow
  , TypeEquals pt Point
  , TrackRecord trackTail trackRow' a
  ) => TrackRecord (Cons name (Aff (Number -> pt -> Array a)) trackTail) trackRow a where
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


type TrackConfigRecord r =
  { trackHeight :: Number
  , padding :: TrackPadding | r }

class TrackConfig
  (confList :: RowList)
  (confRow  :: # Type)
  (trackRow :: # Type)
  | confList -> confRow trackRow
  where
  makeContainersImpl :: RLProxy confList
                     -> Number
                     -> Record confRow
                     -> Effect (Record trackRow)


instance trackConfigNil ::
  ( TypeEquals (Record tr) {}
  ) => TrackConfig Nil cr tr where
  makeContainersImpl _ _ _ = pure $ from {}


instance trackConfigCons ::
  ( IsSymbol name
  , TrackConfig tail confRow' trackRow'
  , Row.Cons name c confRow' confRow
  , Row.Lacks name confRow'
  , Row.Cons name TrackContainer trackRow' trackRow
  , Row.Lacks name trackRow'
  , TypeEquals (TrackConfigRecord we) c
  ) => TrackConfig (Cons name c tail) confRow trackRow where
  makeContainersImpl _ width r = do
    let n = SProxy :: SProxy name

        c :: TrackConfigRecord we
        c = from $ get n r

    cont <- trackContainer
              {width, height: c.trackHeight}
              c.padding
              (Symbol.reflectSymbol n)

    rest <- makeContainersImpl
              (RLProxy :: RLProxy tail)
              width
              (delete n $ from r)

    pure $ insert n cont rest



makeContainers :: ∀ cl cr tr.
                  RowToList cr cl
               => TrackConfig cl cr tr
               => Number
               -> Record cr
               -> Effect (Record tr)
makeContainers = makeContainersImpl (RLProxy :: RLProxy cl)
