module Genetics.Browser.Events
       where

import Prelude
import Data.Argonaut (Json, _Number, _Object, _String, (.?))
import Data.Array (foldMap)
import Data.Either (Either)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Lens (re, (^?))
import Data.Lens.Index (ix)
import Data.Lens.Types (Iso')
import Data.Maybe (Maybe)
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Genetics.Browser.Events.Types (Event(..))
import Genetics.Browser.Units (Bp(..), Chr(..), _Bp, _Chr)

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

newtype JsonEvent = JsonEvent Json

evLocKeys ::
  { locKeys :: Array String
  , chrKeys :: Array String
  , posKeys :: Array String
  }
evLocKeys = { locKeys: ["loc", "locLrs"]
            , chrKeys: ["chr"]
            , posKeys: ["pos"]
            }

newtype EventLocation = EventLocation { chr :: Chr
                                      , pos :: Bp
                                      }

newtype EventRange = EventRange { chr :: Chr
                                , minPos :: Bp
                                , maxPos :: Bp
                                }
newtype EventScore = EventScore Number


parseLocation :: Event -> Maybe EventLocation
parseLocation (Event ev) = do
  chr <- ev ^? ix "chr" <<< _String <#> Chr
  pos <- ev ^? ix "pos" <<< _Number <#> Bp
  pure $ EventLocation { chr, pos }

parseRange :: Event -> Maybe EventRange
parseRange (Event ev) = do
  chr <-    ev ^? ix "chr" <<< _String <#> Chr
  minPos <- ev ^? ix "minPos" <<< _Number <#> Bp
  maxPos <- ev ^? ix "maxPos" <<< _Number <#> Bp
  pure $ EventRange { chr, minPos, maxPos }

-- parseScore :: Event -> Maybe EventScore
-- parseScore (Event ev) = EventScore <$> ev ^? ix "score" <<< _Number
  -- score <- ev ^? ix "score" <<< _Number
  -- pure $ EventScore score

-- eventLocation :: Event -> Either String EventLocation
-- eventLocation (Event { eventData }) = do
--   chr <- eventData .? "chr"
--   pos <- Bp <$> eventData .? "pos"
--   pure $ EventLocation { chr, pos }

-- eventRange :: Event -> Either String EventRange
-- eventRange (Event { eventData }) = do
--   chr <- eventData .? "chr"
--   minPos <- Bp <$> eventData .? "minPos"
--   maxPos <- Bp <$> eventData .? "maxPos"
--   pure $ EventRange { chr, minPos, maxPos }

-- eventScore :: Event -> Either String EventScore
-- eventScore (Event { eventData }) = do
--   score <- eventData .? "score"
--   pure $ EventScore { score }
