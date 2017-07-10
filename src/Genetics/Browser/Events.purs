module Genetics.Browser.Events
       ( Event(..)
       , handleLocation
       , handleRange
       , _eventLocation
       , _eventRange
       , _eventScore
       , EventLocation(..)
       , EventRange(..)
       , EventScore(..)
       , Location(..)
       , Range(..)
       , Score(..)
       )
       where

import Prelude
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, on)
import Genetics.Browser.Units (Bp, Chr)


newtype Event r = Event (Variant r)


type Location = { chr :: Chr
                , pos :: Bp
                }
_eventLocation = (SProxy :: SProxy "location")
type EventLocation r = ( location :: Location | r)
handleLocation :: forall r a.
                  (Location -> a)
               -> (Variant r -> a)
               -> Variant (EventLocation r)
               -> a
handleLocation = on _eventLocation

type Range = { chr :: Chr
             , minPos :: Bp
             , maxPos :: Bp
             }
_eventRange = (SProxy :: SProxy "range")
type EventRange r = ( range :: Range | r)
handleRange :: forall r a.
                  (Range -> a)
               -> (Variant r -> a)
               -> Variant (EventRange r)
               -> a
handleRange = on _eventRange

type Score = { score :: Number }
_eventScore = (SProxy :: SProxy "score")
type EventScore r = ( score :: Score | r)
