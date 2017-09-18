module Genetics.Browser.Events.TrackManager where

import Prelude
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (BusRW)
import Data.Record.Unsafe (unsafeGet)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, expand)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
