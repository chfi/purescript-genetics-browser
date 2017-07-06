module Genetics.Browser.Config
       where

import Data.Argonaut (Json)
import Genetics.Browser.Biodalliance (BrowserConstructor, RenderWrapper)
import Genetics.Browser.Config.Track (TracksMap)

newtype BrowserConfig = BrowserConfig { wrapRenderer :: RenderWrapper
                                      , browser :: BrowserConstructor
                                      , tracks :: TracksMap
                                      }
