module Genetics.Browser.Cytoscape.Types where

import Control.Monad.Eff (kind Effect)
import Data.Argonaut.Core (JObject)

foreign import data Cytoscape :: Type
foreign import data CY :: Effect

foreign import data Element :: Type
foreign import data CyCollection :: Type

foreign import data CyEvent :: Type

foreign import elementJson :: Element -> JObject
