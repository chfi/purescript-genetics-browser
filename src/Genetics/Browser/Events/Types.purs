module Genetics.Browser.Events.Types where

import Data.Argonaut (Json)

type Event = { name :: String
             , evData :: Json
             }

type EventValue = { name :: String
                  , "type" :: String
                  , value :: Json
                  }
