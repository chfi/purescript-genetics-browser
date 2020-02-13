module Genetics.Browser.IndexedDB where

import Prelude

import Effect (Effect)
import Effect.Exception (Error)
import Foreign (Foreign)


-- These aren't actually very safe, as IndexedDB can only store
-- cloneable data, meaning no functions or objects containing
-- functions. We don't need to do that, but it's worth keeping in
-- mind.
foreign import setValueImpl :: ∀ a.
                               String
                            -> a
                            -> Effect Boolean

foreign import setValueImplCB :: ∀ a.
                                 String
                              -> a
                              -> Effect Unit
                              -> (Error -> Effect Unit)
                              -> Effect Unit

foreign import getValueImpl :: String
                            -> (Foreign -> Effect Unit)
                            -> Effect Unit


-- TODO make sure to fail when getting an undefined key
foreign import getValueImplCB :: String
                              -> (Foreign -> Effect Unit)
                              -> (Error -> Effect Unit)
                              -> Effect Unit


foreign import data IndexedDBStore :: Type

-- foreign import initStoreImpl :: Effect Unit

setValue :: ∀ a. String -> a -> Effect Boolean
setValue = setValueImpl
