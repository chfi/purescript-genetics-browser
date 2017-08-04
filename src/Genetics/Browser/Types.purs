module Genetics.Browser.Types
       ( Point
       , View
       , Renderer(..)
       , Quant
       , Biodalliance
       , BD
       ) where

-- TODO: Most of this will be obsolete after BD can be better embedded/
--       native tracks can be used
import Control.Monad.Eff (kind Effect)
import Data.Foreign (Foreign)

type Point = { x :: Number, y :: Number}

type View = { viewStart :: Number
            , scale :: Number
            , height :: Number
            , chr :: String
            }

-- TODO: replace Array Foreign with Array ForeignFeature
-- or similar, for clarity. Could be a newtype or type synonym
newtype Renderer = Renderer (View -> Array Foreign -> Foreign)

type Quant = { min :: Number
             , max :: Number
             }

foreign import data Biodalliance :: Type
foreign import data BD :: Effect
