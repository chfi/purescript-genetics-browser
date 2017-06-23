module Genetics.Browser.Config.Track
       where

import Prelude
import Data.Argonaut (Json, _Object)
import Data.Either (Either(..))
import Data.Foreign (Foreign, toForeign)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))

-- TODO add some BD validation
newtype BDTrackConfig = BDTrackConfig Foreign

makeBDTrack :: ∀ r. { name :: String | r } -> BDTrackConfig
makeBDTrack = (BDTrackConfig <<< toForeign)

validateBDConfig :: Json -> Either String BDTrackConfig
validateBDConfig json = case json ^? _Object <<< ix "name" of
  Nothing -> Left $ "BD track config does not have a name"
  Just c  -> Right $ BDTrackConfig $ toForeign json
