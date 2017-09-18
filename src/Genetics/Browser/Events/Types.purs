module Genetics.Browser.Events.Types where

import Data.Argonaut (Json)

type Event = { eventType :: String
             , event :: Json
             }
