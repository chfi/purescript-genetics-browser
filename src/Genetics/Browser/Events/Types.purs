module Genetics.Browser.Events.Types
       where

import Prelude
import Data.Argonaut.Core (JObject)
import Data.Lens (lens)
import Data.Lens.Record (prop)
import Data.Lens.Types (Lens, Lens')
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Genetics.Browser.Units (Bp)

-- for now the track IDs wil be hardcoded. Later, maybe UUIDs, or generated some other way,
-- with a user-friendly way of defining which tracks are interesting.
newtype TrackId = TrackId String

derive instance newtypeTrackId :: Newtype TrackId _
derive instance eqTrackId :: Eq TrackId
derive instance ordTrackId :: Eq TrackId

type EventData = JObject


  -- add sourceTrack :: TrackId -- (or just TrackType, or even a type parameter...)
  -- or keep track of that in the main Container, as done currently.
newtype Event = Event EventData

_eventData :: Lens' Event EventData
_eventData = lens (\(Event r) -> r) (\_ -> Event )


-- TODO: identify & extract common/usable event data types...
-- newtype EventLocation = EventLocation { chr :: String, pos :: Bp }
-- derive instance newtypeEventLocation :: Newtype EventLocation _



-- newtype EventRange = EventRange { chr :: String, minPos :: Bp, maxPos :: Bp }
-- derive instance newtypeEventRange :: Newtype EventRange _

-- newtype EventScore = EventScore { score :: Number }
-- derive instance newtypeEventScore :: Newtype EventScore _
