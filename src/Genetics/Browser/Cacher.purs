module Genetics.Browser.Cacher where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (get, insert)
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy(..), SProxy(..), from)
import Unsafe.Coerce (unsafeCoerce)



class CacherAff
      input
      (inList :: RowList)
      (inRow :: # Type)
      (outRow :: # Type)
      | inList -> inRow
      , inList -> outRow
      , inList -> input where
  compile :: RLProxy inList
          -> (input -> Record inRow)
          -> Aff { run :: input -> Aff Unit
                 , last :: Record outRow }


instance cacherAffCons ::
  ( IsSymbol  name
  , Row.Lacks name inRow'
  , Row.Cons  name x inRow' inRow
  , Row.Lacks name outRow'
  , Row.Cons  name (Aff x) outRow' outRow
  , CacherAff input inTail inRow' outRow'
  ) => CacherAff input (Cons name x inTail) inRow outRow where
  compile _ fun = do
    let name = SProxy :: _ name

    tail <- compile (RLProxy :: _ inTail) (unsafeCoerce fun)

    avar <- AVar.empty

    let run a = do
          tail.run a
          let val = get name (fun a)
          _ <- AVar.tryTake avar
          AVar.put val avar

    let last = insert name (AVar.read avar) tail.last

    pure { last, run }


instance cacherAffNil ::
  ( TypeEquals (Record outRow) {}
  ) =>
  CacherAff input Nil () outRow where
  compile _ _ = pure { run: \_ -> pure unit
                     , last: (from {}) }



-- | Creates a new cached record using AVars, when provided a function
-- | that outputs a record. `run` updates the cached values in the
-- | record to the output of the function provided with the given
-- | value, and `last` is a record with the same shape as the output
-- | of `fun`, except with each value wrapped in `Aff`, as it reads
-- | the value from the cache AVar.
new :: ∀ inRow inList outRow input.
       RowToList inRow inList
    => CacherAff input inList inRow outRow
    => (input -> Record inRow)
    -> Aff { run :: input -> Aff Unit
           , last :: Record outRow }
new fun = compile (RLProxy :: _ inList) fun
