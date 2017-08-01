module Genetics.Browser.Events.Handler where

import Prelude
import Type.Row
import Data.Record
import Control.Monad.Aff.Bus as Bus
import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, Canceler, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, case_, on, prj, match)

-- 'r' is the wrapped record row
-- 'env' is any extra environment that may be used by the handler (e.g. a Biodalliance instance)
-- 'eff' is the effects used by the handler -- phantom type since I don't know how to do it better
data InputHandler env (eff :: # Effect) (r :: # Type) = InputHandler (Record r)

mkHandler :: forall l a r eff.
             IsSymbol l
          => RowLacks l ()
          => RowCons l (a -> Eff eff Unit) () r
          => SProxy l
          -> (a -> Eff eff Unit)
          -> InputHandler a eff r
mkHandler l a = InputHandler $ insert l a {}

emptyHandler :: forall a. InputHandler a () ()
emptyHandler = InputHandler {}


-- for now, just force all handlers to use the same effects.
insertHandler :: forall l a r1 r2 eff.
             IsSymbol l
          => RowLacks l r1
          => RowCons l (a -> Eff eff Unit) r1 r2
          => SProxy l
          -> (a -> Eff eff Unit)
          -> InputHandler a eff r1
          -> InputHandler a eff r2
insertHandler l a (InputHandler r) = InputHandler $ insert l a r


-- the inputhandler is something that later will be forked.
-- forkHandler :: ∀ a evs eff.
--                a
--             -> BusRW (Variant evs)
--             -> InputHandler a (avar :: AVAR | eff) evs
--             -> Aff (avar :: AVAR | eff) (Canceler ( avar :: AVAR | eff ) )
-- forkHandler env bus h = forkAff $ forever do
--   val <- Bus.read bus
--   liftEff $ h env val

forkHandler :: ∀ env evsV evsR eff.
               -- VariantRecordMatching evsV evsR (Eff eff Unit)
               env
            -> BusRW (Variant evsV)
            -> InputHandler env (avar :: AVAR | eff) evsR
            -> Aff (avar :: AVAR | eff) _
forkHandler env bus (InputHandler h) = forkAff $ forever do
  val <- Bus.read bus
  liftEff $ match h val


type Location = {chr :: String, pos :: Number}
type Range = {pos1 :: Number, pos2 :: Number}
type Ev1 e = (location :: {chr :: String, pos :: Number})
type Ev2 e = (range :: {pos1 :: Number, pos2 :: Number})
type Ev3 e = ( location :: {chr :: String, pos :: Number}
           , range :: {pos1 :: Number, pos2 :: Number})

-- handler1 :: InputHandler Unit Ev1 (console :: CONSOLE)
handler1 :: forall e. (Variant (Ev1 e) -> Unit -> Eff (console :: CONSOLE) Unit)
handler1 v env = case prj (SProxy :: SProxy "location") v of
  Nothing -> pure unit
  Just l  -> do
    log "hello"
    log l.chr

handler1' :: forall env. { location :: Location -> env -> Eff _ Unit }
handler1' = { location }
  where location = \ {chr, pos} env -> do
          log "hello"
          log chr

handler2' :: forall env. { range :: Range -> env -> Eff _ Unit }
handler2' = { range }
  where range = \ {pos1, pos2} env -> do
          log "hello range"


foreign import mergeRecord :: forall r1 r2 r3. Union r1 r2 r3 => Record r1 -> Record r2 -> Record r3

mergeHandlers :: forall r1 r2 r3.
                 Union r1 r2 r3
              => Record r1
              -> Record r2
              -> Record r3
mergeHandlers = mergeRecord



-- handler2 :: InputHandler Unit Ev2 (console :: CONSOLE)
-- handler2 :: forall a eff.
--             a
--          -> (Variant () -> Eff (console :: CONSOLE | eff) Unit)
--          -> Variant Ev2
--          -> Eff (console :: CONSOLE | eff) Unit
handler2 :: forall e. (Variant (Ev2 e) -> Unit -> Eff (console :: CONSOLE) Unit)
handler2 v env = case prj (SProxy :: SProxy "range") v of
  Nothing -> pure unit
  Just l  -> do
    log "hello range"


-- handler3 :: forall e. (Variant (Ev3 e) -> Unit -> Eff (console :: CONSOLE) Unit)
-- handler3 v env = do
--   handler1 v env
--   handler2 v env


-- h3 :: forall ev1 ev2 ev3.
--       Union ev1 ev2 ev3
--    => InputHandler Unit ev1 _
--    -> InputHandler Unit ev2 _
--    -> InputHandler Unit ev3 _
-- h3 ha1 ha2 = ?help

-- data InputHandler2 a evs eff = InputHandler2

-- combineHandlers :: forall a b evs1 evs2 evs3 eff.
--                    Union evs1 evs2 evs3
--                =>
                --    (a -> Variant evs1 -> b)
                -- -> (a -> Variant evs2 -> b)
                -- -> _
                -- -> a -> Variant evs3 -> Eff eff Unit
-- combineHandlers h1 h2 = \a -> (h1 a) <<< (h2 a)

h1 :: forall a evs.
      (Variant evs -> (a -> Eff _ Unit))
   -> (Variant ( butt :: String | evs ) -> (a -> Eff _ Unit))
h1 = on (SProxy :: SProxy "butt") (\butt env -> log "butt")

h2 :: forall a evs.
      (Variant evs -> (a -> Eff _ Unit))
   -> (Variant ( poop :: String | evs ) -> (a -> Eff _ Unit))
h2 = on (SProxy :: SProxy "poop") (\poop env -> log "poop")

comb :: forall a evs.
        (Variant evs -> (a -> Eff _ Unit))
     -> Variant (butt :: String, poop :: String | evs) -> a -> Eff _ Unit
comb = h1 <<< h2
