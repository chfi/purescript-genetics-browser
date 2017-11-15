module Genetics.Browser.Biodalliance.Source where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Promise (Promise)
import Control.Promise as Promise
import Genetics.Browser.Types (Bp, ChrId)

-- this module provides functions for constructing BD-compatible sources

-- a BD source is a function that provides the following function (at least)

-- fetch(chr, min, max, scale, types, pool, callback)

foreign import data Source :: Type -> Type

type FetchFunction f a = ChrId -> Bp -> Bp -> f a

foreign import createSourceImpl :: ∀ a eff.
                                   FetchFunction (Eff eff) (Promise a)
                                -> Source a

createSource :: ∀ eff a.
                FetchFunction (Aff eff) a
             -> Source a
createSource ff = createSourceImpl ff'
  where ff' chr min max = Promise.fromAff $ ff chr min max
