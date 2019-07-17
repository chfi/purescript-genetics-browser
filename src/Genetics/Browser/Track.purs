module Genetics.Browser.Track
       ( class TrackRecord
       , buildTrack
       , makeTrack
       , TrackConfigRecord
       , class TrackConfig
       , makeContainersImpl
       , makeContainers
       , class TrackData
       , fetchDataImpl
       , fetchData
       , class CombineFuns
       , combineFunsImpl
       , combineFuns
       ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.Pair (Pair)
import Effect (Effect)
import Effect.Aff (Aff)
import Genetics.Browser.Canvas (TrackContainer, trackContainer, withLoadingIndicator)
import Genetics.Browser.Coordinates (CoordSysView)
import Genetics.Browser.Layer (TrackPadding)
import Genetics.Browser.Types (Point)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (delete, get, insert)
import Type.Data.Symbol as Symbol
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy(..), SProxy(..), from)
import Unsafe.Coerce (unsafeCoerce)




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



class TrackData
      (fetcherList :: RowList)
      (fetcherRow :: # Type)
      (urlRow :: # Type)
      (dataRow :: # Type)
      | fetcherList -> fetcherRow
      , fetcherList -> urlRow
      , fetcherList -> dataRow where
  fetchDataImpl :: RLProxy fetcherList
            -> Record fetcherRow
            -> Record urlRow
            -> Aff (Record dataRow)


instance trackDataCons ::
  ( IsSymbol  name
  , Row.Lacks name fetcherRow'
  , Row.Cons  name (String -> Aff x) fetcherRow' fetcherRow
  , Monoid x
  , Row.Cons  name (Maybe String) urlRow' urlRow
  , Row.Lacks name dataRow'
  , Row.Cons  name x dataRow' dataRow
  , TrackData fetcherTail fetcherRow' urlRow dataRow'
  ) => TrackData (Cons name (String -> Aff x) fetcherTail)
                 fetcherRow
                 urlRow
                 dataRow where
  fetchDataImpl _ fetchers urls = do
    let name = SProxy :: _ name

    rest <- fetchDataImpl (RLProxy :: _ fetcherTail)
                          (delete name fetchers)
                          urls

    dataset <- foldMap (get name fetchers) (get name urls)

    pure $ insert name dataset rest


instance trackDataNil ::
  ( TypeEquals (Record dataRow) {}
  ) => TrackData Nil () urlRow dataRow where
  fetchDataImpl _ _ _ = pure $ from {}


-- | `fetchData` puts a loading indicator in the provided TrackContainer,
-- | fetches the data in the provided `urlRow` record, and parses it
-- | using the parsers in the provided `parserRow` record.
fetchData :: ∀ fetcherRow fetcherList urlRow urlList dataRow.
             RowToList fetcherRow fetcherList
          => RowToList urlRow urlList
          => TrackData fetcherList fetcherRow urlRow dataRow
          => TrackContainer
          -> Record fetcherRow
          -> Record urlRow
          -> Aff (Record dataRow)
fetchData tc fs urls = withLoadingIndicator tc
                       $ fetchDataImpl (RLProxy :: _ fetcherList)
                                       fs
                                       urls



-- | Combine a record of functions from records,
-- | into a function from a record of their union,
-- | to a record of their outputs.
-- | E.g. `{f :: Record r1 -> a, g :: Record r2 -> b}`
-- | into `Record (r1 + r2) -> { f :: a, g :: b }`
class CombineFuns
  (funsList :: RowList)
  (funsRow :: # Type)
  (argRow :: # Type)
  (outRow :: # Type)
  | funsList -> funsRow
  , funsList -> argRow
  , funsList -> outRow where
  combineFunsImpl :: RLProxy funsList
              -> Record funsRow
              -> Record argRow
              -> Record outRow


instance combineFunCons ::
  ( IsSymbol  name
  , Row.Lacks name funsRow'
  , Row.Cons  name (Record r -> a) funsRow' funsRow
  , Row.Union argRow' r argRowUnion
  , Row.Nub   argRowUnion argRowNubbed
  , Row.Lacks name outRow'
  , Row.Cons  name a outRow' outRow
  , CombineFuns funsTail funsRow' argRow' outRow'
  ) => CombineFuns (Cons name (Record r -> a) funsTail)
                  funsRow
                  argRowNubbed
                  outRow where
  combineFunsImpl _ funs args =
    let name = SProxy :: _ name

        thoseArgs :: Record argRow'
        thoseArgs = unsafeCoerce args

        theseArgs :: Record r
        theseArgs = unsafeCoerce args

        rest = combineFunsImpl (RLProxy :: _ funsTail)
                               (delete name funs)
                               thoseArgs
        fn = (get name funs)

    in (insert name (fn theseArgs) rest)


instance combineFunNil ::
  ( TypeEquals (Record outRow) {}
  ) => CombineFuns Nil () () outRow where
  combineFunsImpl _ _ _ = from {}


-- | Note! this isn't actually type safe yet.
-- | If two functions use arguments with the same label
-- | but different types, things break...
combineFuns :: ∀ funsList funsRow argRow outRow.
               RowToList funsRow funsList
            => CombineFuns funsList funsRow argRow outRow
            => Record funsRow
            -> Record argRow
            -> Record outRow
combineFuns funs args = combineFunsImpl (RLProxy :: _ funsList) funs args
