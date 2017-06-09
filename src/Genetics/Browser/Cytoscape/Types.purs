module Genetics.Browser.Cytoscape.Types where

import Control.Monad.Eff (kind Effect)
import Data.Argonaut (Json)
import Data.Argonaut.Core (JObject)

foreign import data Cytoscape :: Type
foreign import data CY :: Effect

foreign import data Element :: Type

foreign import data CyEvent :: Type


foreign import elementJObject :: Element -> JObject
foreign import elementJson :: Element -> Json
