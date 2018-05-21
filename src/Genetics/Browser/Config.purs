module Genetics.Browser.Config where

import Prelude

import Color (Color)
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Ref as Ref
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

class EffConfigRecord
  ( lc :: RowList )
  ( lf :: RowList )
  -- ( lo :: RowList )
  ( rc :: # Type  )
  ( rf :: # Type  )
  ( ro :: # Type  )
  | lc -> rc
  , lf -> rf
  , lc lf -> ro
  -- , lc lf -> lo, lf lo -> lc, lc lo -> lf
    where
      doThingImpl :: RLProxy lc
                  -> RLProxy lf
                  -> Record rc
                  -> Record rf
                  -> Eff (ref :: REF) (Record ro)


instance nilEffConfigRecord
  :: EffConfigRecord Nil Nil () () () where
    doThingImpl _ _ _ _ = pure {}

instance consEffConfigRecord
  :: ( IsSymbol l
     , RowCons  l a        trashC rc
     , RowCons  l (a -> b) trashF rf
     , RowCons  l (RC a b)       ro'    ro
     , RowLacks l          ro'
     , EffConfigRecord tc tf rc rf ro'
     ) => EffConfigRecord (Cons l a tc) (Cons l (a -> b) tf) rc rf ro
  where
    doThingImpl _ _ rc rf = do
      let c = Record.get (SProxy :: SProxy l) rc
          f = Record.get (SProxy :: SProxy l) rf
      o <- reactiveConfig' f c
      t <- doThingImpl (RLProxy :: RLProxy tc) (RLProxy :: RLProxy tf) rc rf
      pure $ Record.insert (SProxy :: SProxy l) o t



doThing :: forall lc lf rc rf ro.
           EffConfigRecord lc lf rc rf ro
        => Record rc
        -> Record rf
        -> Eff _ (Record ro)
doThing = doThingImpl (RLProxy :: RLProxy lc) (RLProxy :: RLProxy lf)


test :: forall lc lf rc rf ro.
        RowToList BothR lc
     => RowToList BothRF lf
     => EffConfigRecord lc lf BothR BothRF ro
     => Eff _ (Record ro)
test = doThing aaa bbb

-- test :: _
-- test = doThing aaa bbb
