module Genetics.Browser.Events
       where

import Data.Argonaut ((.?))
import Data.Either (Either)
import Genetics.Browser.Events.Types (Event(..), EventLocation(..), EventRange(..), EventScore(..))
import Genetics.Browser.Units (Bp(..))
import Prelude

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

evLocKeys ::
  { locKeys :: Array String
  , chrKeys :: Array String
  , posKeys :: Array String
  }
evLocKeys = { locKeys: ["loc", "locLrs"]
            , chrKeys: ["chr"]
            , posKeys: ["pos"]
            }

eventLocation :: Event -> Either String EventLocation
eventLocation (Event { eventData }) = do
  chr <- eventData .? "chr"
  pos <- Bp <$> eventData .? "pos"
  pure $ EventLocation { chr, pos }

eventRange :: Event -> Either String EventRange
eventRange (Event { eventData }) = do
  chr <- eventData .? "chr"
  minPos <- Bp <$> eventData .? "minPos"
  maxPos <- Bp <$> eventData .? "maxPos"
  pure $ EventRange { chr, minPos, maxPos }

eventScore :: Event -> Either String EventScore
eventScore (Event { eventData }) = do
  score <- eventData .? "score"
  pure $ EventScore { score }
