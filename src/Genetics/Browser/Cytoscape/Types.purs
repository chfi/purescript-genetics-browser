module Genetics.Browser.Cytoscape.Types where

import Control.Monad.Eff (kind Effect)
import Data.Argonaut (Json)
import Data.Argonaut.Core (JObject)

-- | The Cytoscape graph and effect for functions that interact with it
foreign import data Cytoscape :: Type
foreign import data CY :: Effect

-- | Cytoscape elements (Edges and Nodes)
foreign import data Element :: Type

foreign import data CyEvent :: Type

-- | Return the JSON representation of an element
foreign import elementJObject :: Element -> JObject
foreign import elementJson :: Element -> Json
