module Genetics.Browser.Config.Track
       where


import Prelude
import Data.Foreign (Foreign, toForeign)


newtype BDTrackConfig = BDTrackConfig Foreign

makeBDTrack :: ∀ r. { name :: String, renderer :: String | r } -> BDTrackConfig
makeBDTrack = (BDTrackConfig <<< toForeign)
