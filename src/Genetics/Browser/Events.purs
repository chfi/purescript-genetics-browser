module Genetics.Browser.Events
       ( JsonEvent(..)
       , EventLocation(..)
       , EventRange(..)
       , EventScore(..)
       )
       where

import Prelude
import Data.Argonaut (Json, _Number, _Object, _String)
import Data.Array (foldMap)
import Data.Foldable (class Foldable)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Lens (re, (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe)
import Data.Maybe.First (First(..))
import Data.Newtype (class Newtype, unwrap)
import Genetics.Browser.Units (Bp, Chr, _Bp, _Chr)

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

newtype JsonEvent = JsonEvent Foreign

derive instance newtypeJsonEvent :: Newtype JsonEvent _


newtype EventLocation = EventLocation { chr :: Chr
                                      , pos :: Bp
                                      }

derive instance genericEventLocation :: Generic EventLocation _

instance encodeEventLocation :: Encode EventLocation where
  encode = genericEncode defaultOptions

instance decodeEventLocation :: Decode EventLocation where
  decode = genericDecode defaultOptions


parseEventLocation :: ∀ f r.
                      Foldable f
                   => Functor f
                   => { posKeys :: f String, chrKeys :: f String | r }
                   -> Json
                   -> Maybe EventLocation
parseEventLocation {chrKeys, posKeys} obj = do
    chr <- unwrap $ foldMap First $ (\k -> obj ^?
                                           _Object <<< ix k <<<
                                           _String <<< re _Chr) <$> chrKeys
    pos <- unwrap $ foldMap First $ (\k -> obj ^?
                                           _Object <<< ix k <<<
                                           _Number <<< re _Bp) <$> posKeys
    pure $ EventLocation { chr, pos }



newtype EventRange = EventRange { chr :: Chr
                                , minPos :: Bp
                                , maxPos :: Bp
                                }

derive instance genericEventRange :: Generic EventRange _

instance encodeEventRange :: Encode EventRange where
  encode = genericEncode defaultOptions

instance decodeEventRange :: Decode EventRange where
  decode = genericDecode defaultOptions


parseEventRange :: ∀ f r.
                      Foldable f
                   => Functor f
                   => { chrKeys :: f String, minPosKeys :: f String , maxPosKeys :: f String | r }
                   -> Json
                   -> Maybe EventRange
parseEventRange {chrKeys, minPosKeys, maxPosKeys} obj = do
    chr <- unwrap $ foldMap First $ (\k -> obj ^?
                                           _Object <<< ix k <<<
                                           _String <<< re _Chr) <$> chrKeys
    minPos <- unwrap $ foldMap First $ (\k -> obj ^?
                                           _Object <<< ix k <<<
                                           _Number <<< re _Bp) <$> minPosKeys
    maxPos <- unwrap $ foldMap First $ (\k -> obj ^?
                                           _Object <<< ix k <<<
                                           _Number <<< re _Bp) <$> maxPosKeys
    pure $ EventRange { chr, minPos, maxPos }


newtype EventScore = EventScore Number
