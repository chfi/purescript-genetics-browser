module Genetics.Browser.Biodalliance.Source where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Promise (Promise)
import Control.Promise as Promise
import Genetics.Browser.Units (Bp, Chr)

-- this module provides functions for constructing BD-compatible sources

-- a BD source is a function that provides the following function (at least)

-- fetch(chr, min, max, scale, types, pool, callback)

-- they also extend FeatureSourceBase...
-- how to provide that, I wonder
-- probably best to provide it as a function argument.

foreign import data ForeignSourceBase :: Type

foreign import data Source :: Type -> Type

type FetchFunction f a = Chr -> Bp -> Bp -> f a

foreign import createSourceImpl :: ∀ a eff.
                                   ForeignSourceBase
                                -> FetchFunction (Eff eff) (Promise a)
                                -> Source a

createSource :: ∀ a.
                ForeignSourceBase
             -> FetchFunction (Aff _) a
             -> Source a
createSource fsb ff = createSourceImpl fsb ff'
  where ff' chr min max = Promise.fromAff $ ff chr min max
