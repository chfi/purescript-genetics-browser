module Genetics.Browser.Cached
       ( Cached
       , cache
       , input
       , output
       , applyCache
       , modInput
       , clone
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Eff.Ref as Ref
import Data.Lens.Internal.Tagged (Tagged(..))
import Data.Record as Record
import Type.Prelude (class IsSymbol, class RowLacks, SProxy(SProxy))


-- | A cached function from `a` to `b`. Create with `cache`, "apply"
-- | (i.e. change the input) with `applyCache`, and retrieve the /latest/
-- | input and output with functions `input` and `output`.
data Cached a b = Cached (Ref a) (Ref b) (a -> b)

class (RowCons s (Cached a b) r r', RowLacks s r) <= RowCached (s :: Symbol) a b r r'

insert' :: ∀ s a r r'.
           IsSymbol s
        => RowCons  s a r r'
        => RowLacks s r
        => Tagged (SProxy s) a
        -> Record r
        -> Record r'
insert' (Tagged a) r = Record.insert (SProxy :: SProxy s) a r


tagSProxy :: ∀ s a.
             Tagged (SProxy s) a -> SProxy s
tagSProxy _ = SProxy



-- | Provided a function, and a value to apply it to, creates a "cached"
-- | instance of the function, where the current input and output are stored
-- | in Refs, and the input can be modified.
cache :: ∀ a b.
         (a -> b)
      -> a
      -> Eff _ (Cached a b)
cache f i = do
  let o = f i
  inRef  <- Ref.newRef i
  outRef <- Ref.newRef o
  pure $ Cached inRef outRef f

input :: ∀ a b.
          Cached a b -> Eff _ a
input (Cached i _ _) = Ref.readRef i

output :: ∀ a b.
           Cached a b -> Eff _ b
output (Cached _ o _) = Ref.readRef o

applyCache :: ∀ a b.
              Cached a b -> a -> Eff _ Unit
applyCache (Cached iR oR f) i = do
  Ref.writeRef iR i
  Ref.writeRef oR (f i)
  pure unit

modInput :: ∀ a b.
            (a -> a) -> Cached a b -> Eff _ Unit
modInput g (Cached iR oR f) = do
  i' <- Ref.readRef iR
  o' <- Ref.readRef oR
  let i = g i'
      o = f i
  Ref.writeRef iR i
  Ref.writeRef oR o
  pure unit

clone :: ∀ a b.
         Cached a b -> Eff _ (Cached a b)
clone (Cached iR oR f) = do
  i <- Ref.readRef iR
  o <- Ref.readRef oR
  inRef  <- Ref.newRef i
  outRef <- Ref.newRef o
  pure $ Cached inRef outRef f
