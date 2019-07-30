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
import Prim.Boolean (False, True, kind Boolean)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (delete, get, insert)
import Type.Data.Boolean as Boolean
import Type.Data.Symbol as Symbol
import Type.Prelude (class IsSymbol, class TypeEquals, Proxy(..), RLProxy(..), RProxy(..), SProxy(..), from)
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



class BuildLayers
  (layers :: # Type)
  (output :: # Type)
  | layers -> output



class GetBrowserConfig
  (totalConfig :: # Type)
  (browser :: # Type)
  | totalConfig -> browser where
    getBrowserConfigImpl :: Record totalConfig
                         -> Record browser

instance getBrowserConfig ::
  ( IsSymbol sBrowser
  , TypeEquals (SProxy sBrowser) (SProxy "browser")
  , IsSymbol sLayers
  , TypeEquals (SProxy sLayers) (SProxy "layers")

  , Row.Cons sBrowser (Record browser') totalConfig' totalConfig
  , Row.Lacks sBrowser totalConfig'

  , Row.Cons sLayers layers browser browser'
  , Row.Lacks sLayers browser
  ) => GetBrowserConfig totalConfig browser where
  getBrowserConfigImpl total =
    let browser' :: Record browser'
        browser' = get (from (SProxy :: _ "browser")) total
    in delete (from (SProxy :: _ "layers")) browser'


class GetTrackConfig
  (totalConfig :: # Type)
  (track :: # Type)
  (trackName :: Symbol)
  | totalConfig trackName -> track where
    getTrackConfigImpl :: Record totalConfig
                       -> SProxy trackName
                       -> Record track

instance getTrackConfig ::
  ( IsSymbol sTracks
  , TypeEquals (SProxy sTracks) (SProxy "tracks")
  , IsSymbol sLayers
  , TypeEquals (SProxy sLayers) (SProxy "layers")
  , IsSymbol trackName

  , Row.Cons sTracks (Record tracks) totalConfig' totalConfig
  , Row.Lacks sTracks totalConfig'

  , Row.Cons trackName (Record thisTrack') tracks' tracks
  , Row.Lacks trackName tracks'

  , Row.Cons sLayers layers thisTrack thisTrack'
  , Row.Lacks sLayers thisTrack

  ) => GetTrackConfig totalConfig thisTrack trackName where
    getTrackConfigImpl total _ =
      let tracks' :: Record tracks
          tracks' = get (from (SProxy :: _ "tracks")) total

          thisTrack :: Record thisTrack'
          thisTrack = get (from (SProxy :: _ trackName)) tracks'

      in delete (from (SProxy :: _ "layers")) thisTrack



class GetLayerConfig
  (totalConfig :: # Type)
  (layer :: # Type)
  (trackName :: Symbol)
  (layerName :: Symbol)
  | totalConfig trackName layerName -> layer where
    getLayerConfigImpl :: Record totalConfig
                       -> SProxy trackName
                       -> SProxy layerName
                       -> Record layer

instance getLayerConfig ::
  ( IsSymbol sTracks
  , TypeEquals (SProxy sTracks) (SProxy "tracks")
  , IsSymbol sLayers
  , TypeEquals (SProxy sLayers) (SProxy "layers")
  , IsSymbol trackName
  , IsSymbol layerName

  , Row.Cons sTracks (Record tracks) totalConfig' totalConfig
  , Row.Lacks sTracks totalConfig'

  , Row.Cons trackName (Record thisTrack) tracks' tracks
  , Row.Lacks trackName tracks'

  , Row.Cons sLayers (Record layers) thisTrack' thisTrack
  , Row.Lacks sLayers thisTrack

  , Row.Cons layerName (Record thisLayer) layers' layers
  , Row.Lacks layerName layers'
  ) => GetLayerConfig totalConfig thisLayer trackName layerName where
    getLayerConfigImpl total _ _ =
      let tracks' :: Record tracks
          tracks' = get (from (SProxy :: _ "tracks")) total

          thisTrack :: Record thisTrack
          thisTrack = get (from (SProxy :: _ trackName)) tracks'

          layers :: Record layers
          layers = get (from (SProxy :: _ "layers")) thisTrack

      in get (from (SProxy :: _ layerName)) layers


newtype LayerDef browser track layer a
  = LayerDef ({ browserConfig :: Record browser
              , trackConfig :: Record track
              , layerConfig :: Record layer }
              -> a)


class ApplyLayerDef
  (browser :: # Type)
  (track :: # Type)
  (layer :: # Type)
  (totalConfig :: # Type)
  (trackName :: Symbol)
  (layerName :: Symbol)
  a
  | trackName layerName totalConfig -> track layer where
    applyLayerDefImpl :: LayerDef browser track layer a
                      -> SProxy trackName
                      -> SProxy layerName
                      -> Record totalConfig
                      -> a

instance applyLayerDefInst ::
  ( IsSymbol trackName
  , IsSymbol layerName
  , GetBrowserConfig totalConfig browser
  , GetTrackConfig totalConfig track trackName
  , GetLayerConfig totalConfig layer trackName layerName
  ) => ApplyLayerDef browser track layer totalConfig trackName layerName a where
    applyLayerDefImpl (LayerDef f) trackName layerName total =
      let browserConfig = getBrowserConfigImpl total
          trackConfig = getTrackConfigImpl total trackName
          layerConfig = getLayerConfigImpl total trackName layerName
      in f { browserConfig, trackConfig, layerConfig }


applyLayerDef :: ∀ browser track layer trackName layerName config a.
                 IsSymbol trackName
              => IsSymbol layerName
              => ApplyLayerDef browser track layer config trackName layerName a
              => LayerDef browser track layer a
              -> SProxy trackName
              -> SProxy layerName
              -> Record config
              -> a
applyLayerDef = applyLayerDefImpl



-- | Class for checking if a RowList has a "conflict" with a
-- | given label and type. If the list lacks the label, or
-- | has the label with the same type, return False; if it
-- | has the label, but with a different type, return True.
class ConflictsWith
  (list :: RowList)
  (label :: Symbol)
  (t :: Type)
  (result :: Boolean)
  | list label t -> result

-- no conflict base case
instance conflictsWithNil ::
  ConflictsWith Nil label t False

-- same label and type, no conflict, recurse
instance conflictsWithCons1 ::
  ( ConflictsWith tail label t tailResult
  ) => ConflictsWith (Cons label  t  tail) label t tailResult

-- found conflict base case
else instance conflictsWithCons3 ::
  ConflictsWith (Cons label  t' tail) label t True

-- different label, same or different type, recurse
else instance conflictsWithCons2 ::
  ( ConflictsWith tail label t tailResult
  ) =>  ConflictsWith (Cons label' t' tail) label t tailResult

hasConflict :: ∀ row list l a .
                 RowToList row list
              => IsSymbol l
              => ConflictsWith list l a True
              => RProxy row
              -> SProxy l
              -> Proxy a
              -> Unit
hasConflict _ _ _ = unit


noConflict :: ∀ row list l a .
                 RowToList row list
              => IsSymbol l
              => ConflictsWith list l a False
              => RProxy row
              -> SProxy l
              -> Proxy a
              -> Unit
noConflict _ _ _ = unit

-- | Returns True if there are duplicates of any label with different types
class ConflictingList
  (list :: RowList)
  (result :: Boolean)
  | list -> result

instance conflictingListNil :: ConflictingList Nil False

instance conflictingListCons ::
  ( ConflictsWith tail label t conflicts
  , ConflictingList tail tailResult
  , Boolean.Or conflicts tailResult result
  ) => ConflictingList (Cons label t tail) result

doesConflict :: ∀ row list.
                RowToList row list
             => ConflictingList list True
             => RProxy row
             -> Unit
doesConflict _ = unit
