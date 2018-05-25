module Genetics.Browser.Config where

import Prelude

import Color (Color, rotateHue)
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Reader (class MonadReader, ask)
import Data.Monoid (mempty)
import Data.Record as Record
import Data.Symbol (class IsSymbol, SProxy(..))
import Graphics.Drawing (Drawing)
import Type.Prelude (class RowLacks, class RowToList, RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)



type VScale =
  { color          :: Color
  , hPad           :: Number
  , numSteps       :: Int
  , yAxisFontSize  :: Int
  , yLabelFontSize :: Int }

_vscale = SProxy :: SProxy "vscale"

drawVScale :: VScale -> Drawing
drawVScale c = mempty

drawVScale' :: forall m r.
               MonadReader {vscale :: VScale | r} m
            => m Drawing
drawVScale' = do
  conf <- _.vscale <$> ask
  pure $ drawVScale conf


type Legend =
  { hPad     :: Number
  , vPad     :: Number
  , fontSize :: Int }

_legend = SProxy :: SProxy "legend"

drawLegend :: Legend -> Drawing
drawLegend _ = mempty

drawLegend' :: forall m r.
               MonadReader {legend :: Legend | r} m
            => m Drawing
drawLegend' = drawLegend <<< _.legend <$> ask



type BothR =
  ( vscale :: VScale
  , legend :: Legend )


type Both = Record BothR

aaa :: Both
aaa = { vscale: unsafeCoerce unit
      , legend: unsafeCoerce unit }

type BothRF =
  ( vscale :: VScale -> Drawing
  , legend :: Legend -> Drawing )

type BothF = Record BothRF

bbb :: BothF
bbb = { vscale: unsafeCoerce unit
      , legend: unsafeCoerce unit }

drawBoth :: Both -> Drawing
drawBoth c = drawVScale c.vscale <> drawLegend c.legend

drawBoth' :: forall m r.
            MonadReader Both m
         => m Drawing
drawBoth' = append <$> drawVScale' <*> drawLegend'
  -- a <- drawVScale'
  -- b <- drawLegend'
  -- pure $ a <> b




reactiveConfig :: forall a b.
                  (a -> b)
               -> a
               -> Eff _ { run :: Eff _ b
                        , getConfig :: Eff _ a
                        , modConfig :: (a -> a) -> Eff _ Unit }
reactiveConfig f i = do
  curArg <- Ref.newRef i

  let run = f <$> Ref.readRef curArg
      getConfig = Ref.readRef curArg
      modConfig = Ref.modifyRef curArg

  pure { run, getConfig, modConfig }


newtype RC a b =
  RC { run :: Eff (ref :: REF) b
     , getConfig :: Eff (ref :: REF) a
     , modConfig :: (a -> a) -> Eff (ref :: REF) Unit }


reactiveConfig' :: forall a b.
                  (a -> b)
               -> a
               -> Eff _ (RC a b)
reactiveConfig' f i = RC <$> reactiveConfig f i


doThing :: forall m a a' b b' t.
           Apply m
        => { theA :: a -> a'
           , theB :: b -> b' }
        -> { theA :: a
           , theB :: b }
        -> (forall x y. (x -> y) -> x -> m (t x y))
        -> m { theA :: t a a'
             , theB :: t b b'}
doThing funs confs g =
  { theA: _, theB: _ }
  <$> g funs.theA confs.theA
  <*> g funs.theB confs.theB


class RCSeqRecord
  ( rl   :: RowList )
  ( row  :: # Type  )
  ( row' :: # Type  )
  | rl -> row row'
  where
    rcSeqRecordImpl :: forall e.
                       RLProxy rl
                    -> Record row
                    -> Eff (ref :: REF | e) (Record row')

instance rcSeqRecordNil
  :: RCSeqRecord Nil row () where
    rcSeqRecordImpl _ _ = pure {}

instance rcSeqRecordCons
  :: ( IsSymbol name
     , RowCons  name (Eff e ty) trash row
     , RCSeqRecord tail row tailRow'
     , RowLacks name tailRow'
     , RowCons name ty tailRow' row'
     ) => RCSeqRecord (Cons name (Eff e ty) tail) row row'
     where
       rcSeqRecordImpl _ r = do
         let n = SProxy :: SProxy name
         a <- unsafeCoerceEff $ Record.get n r
         tail <- rcSeqRecordImpl (RLProxy :: RLProxy tail) r
         pure $ Record.insert n a tail




-- class RCApplyRecord
--   ( li :: RowList )
--   ( lf :: RowList )
--   ( ri :: # Type  )
--   ( rf :: # Type  )
--   ( ro :: # Type  )
--   | li -> ri
--   , lf -> rf
--   , li lf -> ro
--     where
--       rcApplyRecordImpl :: forall e.
--                            RLProxy li
--                         -> RLProxy lf
--                         -> Record ri
--                         -> Record rf
--                         -> Eff (ref :: REF | e) (Record ro)

-- instance rcApplyRecordNil
--   :: RCApplyRecord Nil Nil ri rf () where
--     rcApplyRecordImpl _ _ _ _ = pure {}

-- instance rcApplyRecordCons
--   :: ( IsSymbol name
--      , RowCons  name i trashI ri
--      , RowCons  name (i -> Eff (ref :: REF) o) trashF rf
--      , Row

-- class HKTApplyRecord
--   ( li :: RowList )
--   ( lf :: RowList )
--   ( ri :: # Type  )
--   ( rf :: # Type  )
--   ( ro :: # Type  )
--   ( f  ::   Type -> Type -> Type )
--   | li -> ri
--   , lf -> rf
--   , li lf -> ro
--     where
--       applyRecordImpl :: forall x y t.
--                          Applicative t
--                       => RLProxy li
--                       -> RLProxy lf
--                       -> (x -> y -> f x y)
--                       -> Record ri
--                       -> Record rf
--                       -> t (Record ro)


-- instance hktApplyRecordNilRC
--   :: HKTApplyRecord Nil Nil ri rf () RC where
--     applyRecordImpl _ _ _ _ _ = pure {}


-- instance hktApplyRecordCons
--   :: ( IsSymbol k
--      , RowCons  k i        trashI ri
--      , RowCons  k (i -> o) trashF rf
--      , RowCons  k (f i o ) rot    ro
--      , RowLacks k rot
--      , HKTApplyRecord ti tf ri rf rot RC
--      ) => HKTApplyRecord (Cons k i ti) (Cons k (i -> o) tf) ri rf ro RC
--      where
--        applyRecordImpl _ _ f ri rf =
--          let name = SProxy :: SProxy k
--              i = Record.get name ri
--              g = Record.get name rf
--              x :: f i o
--              x = f i g



--       doThingImpl :: RLProxy lc
--                   -> RLProxy lf
--                   -> Record rc
--                   -> Record rf
--                   -> Eff (ref :: REF) (Record ro)




-- class EffConfigRecord
--   ( lc :: RowList )
--   ( lf :: RowList )
--   -- ( lo :: RowList )
--   ( rc :: # Type  )
--   ( rf :: # Type  )
--   ( ro :: # Type  )
--   | lc -> rc
--   , lf -> rf
--   , lc lf -> ro
--   -- , lc lf -> lo, lf lo -> lc, lc lo -> lf
--     where
--       doThingImpl :: RLProxy lc
--                   -> RLProxy lf
--                   -> Record rc
--                   -> Record rf
--                   -> Eff (ref :: REF) (Record ro)


-- instance nilEffConfigRecord
--   :: EffConfigRecord Nil Nil () () () where
--     doThingImpl _ _ _ _ = pure {}

-- instance consEffConfigRecord
--   :: ( IsSymbol l
--      , RowCons  l a        trashC rc
--      , RowCons  l (a -> b) trashF rf
--      , RowCons  l (RC a b)       ro'    ro
--      , RowLacks l          ro'
--      , EffConfigRecord tc tf rc rf ro'
--      ) => EffConfigRecord (Cons l a tc) (Cons l (a -> b) tf) rc rf ro
--   where
--     doThingImpl _ _ rc rf = do
--       let c = Record.get (SProxy :: SProxy l) rc
--           f = Record.get (SProxy :: SProxy l) rf
--       o <- reactiveConfig' f c
--       t <- doThingImpl (RLProxy :: RLProxy tc) (RLProxy :: RLProxy tf) rc rf
--       pure $ Record.insert (SProxy :: SProxy l) o t



-- doThing :: forall lc lf rc rf ro.
--            EffConfigRecord lc lf rc rf ro
--         => Record rc
--         -> Record rf
--         -> Eff _ (Record ro)
-- doThing = doThingImpl (RLProxy :: RLProxy lc) (RLProxy :: RLProxy lf)


-- test :: forall lc lf rc rf ro.
--         RowToList BothR lc
--      => RowToList BothRF lf
--      => EffConfigRecord lc lf BothR BothRF ro
--      => Eff _ (Record ro)
-- test = doThing aaa bbb

-- test :: _
-- test = doThing aaa bbb
