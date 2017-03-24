module Genetics.Browser.Types where

import Data.Foreign (Foreign)

type Point = { x :: Number, y :: Number}

type View = { viewStart :: Number
            , scale :: Number
            , height :: Number
            }

-- TODO: replace Array Foreign with Array ForeignFeature
-- or similar, for clarity. Could be a newtype or type synonym
type Renderer = View -> Array Foreign -> Foreign
