module Genetics.Browser.Events
       where

import Prelude
import Data.Newtype
import Data.StrMap
import Data.Argonaut (Json, (.?))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Genetics.Browser.Feature.Foreign (parseFeatureLocation)
import Genetics.Browser.Units (Bp(..))

-- An Event comes from some track, and carries some information.
-- depending on the track (?), the information may differ.
-- can a single track send multiple types of events?
-- yes -- e.g. cytoscape can, at the *least*, have both nodes and edges,
-- and they carry different data.
{-
The data flow should be
(Cy.js|BD|w/e) -> PS (Halogen) -> Parse into Event -> Send in coroutine/pipeline
-> Main container receives Event
-> Main container resends Events to relevant subscribers
-> Subscriber receives Event and acts accordingly

Thus, it will be parsed from JSON into a PS data structure at the very outer edge,
and remain type safe like that.

-}

-- so, source track and event types are orthogonal.

-- for now the track IDs wil be hardcoded. Later, maybe UUIDs, or generated some other way,
-- with a user-friendly way of defining which tracks are interesting.
newtype TrackId = TrackId String

derive instance newtypeTrackId :: Newtype TrackId _
derive instance eqTrackId :: Eq TrackId
derive instance ordTrackId :: Eq TrackId

-- The data in an event can be anything json-serializable.
-- The main container tries to parse the event data, to find e.g. a location or loc range,
-- and dispatches accordingly.

-- It'd be nice to have this on the type level. Maybe possible using rows and proxies?
-- need to research.
type EventData = StrMap Json

newtype Event = Event { sourceTrack :: TrackId
                      , eventData :: EventData
                      }

-- TODO: identify & extract common/usable event data types...
newtype EventLocation = EventLocation { chr :: String, pos :: Bp }
derive instance newtypeEventLocation :: Newtype EventLocation _

newtype EventRange = EventRange { chr :: String, lPos :: Bp, rPos :: Bp }
derive instance newtypeEventRange :: Newtype EventRange _


evLocKeys ::
  { locKeys :: Array String
  , chrKeys :: Array String
  , posKeys :: Array String
  }
evLocKeys = { locKeys: []
            , chrKeys: []
            , posKeys: []
            }

eventLocation :: Event -> Maybe EventLocation
eventLocation (Event { sourceTrack, eventData }) = case parseFeatureLocation evLocKeys eventData of
  Left err  -> Nothing
  Right loc -> Just $ EventLocation loc
