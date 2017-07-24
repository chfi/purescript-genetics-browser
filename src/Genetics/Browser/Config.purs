module Genetics.Browser.Config
       where

import Prelude
import Data.Either (Either)
import Data.Foreign (Foreign, F, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Genetics.Browser.Biodalliance (BrowserConstructor, RenderWrapper)
import Genetics.Browser.Config.Track (TracksMap)
import Unsafe.Coerce (unsafeCoerce)

newtype BrowserConfig = BrowserConfig { wrapRenderer :: RenderWrapper
                                      , browser :: BrowserConstructor
                                      , tracks :: TracksMap
                                      }



parseBrowserConfig :: Foreign -> F BrowserConfig
parseBrowserConfig f = do
  wrapRenderer <- f ! "wrapRenderer" >>= unsafeReadTagged "Function"
  browser <- f ! "browser" >>= unsafeReadTagged "Function"
  tracks <- unsafeCoerce <$> f ! "tracks"
  pure $ BrowserConfig { wrapRenderer, browser, tracks }
