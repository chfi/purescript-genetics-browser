module Genetics.Browser.Config
       where

import Data.Argonaut (Json)
import Genetics.Browser.Biodalliance (BrowserConstructor, RenderWrapper)

newtype BrowserConfig = BrowserConfig { wrapRenderer :: RenderWrapper
                                      , browser :: BrowserConstructor
                                      , tracks :: Json
                                      }
