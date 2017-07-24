module Genetics.Browser.Config
       where

import Prelude
import Data.Array (null)
import Data.Foldable (all)
import Data.Foreign (F, Foreign, ForeignError(..), fail, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.Maybe (isJust)
import Genetics.Browser.Biodalliance (BrowserConstructor, RenderWrapper)
import Genetics.Browser.Config.Track (TracksMap, readTrackType)
import Unsafe.Coerce (unsafeCoerce)

newtype BrowserConfig = BrowserConfig { wrapRenderer :: RenderWrapper
                                      , browser :: BrowserConstructor
                                      , tracks :: TracksMap
                                      }



parseBrowserConfig :: Foreign -> F BrowserConfig
parseBrowserConfig f = do
  wrapRenderer <- f ! "wrapRenderer" >>= unsafeReadTagged "Function"
  browser <- f ! "browser" >>= unsafeReadTagged "Function"
  tracks <- f ! "tracks"
  tracksKeys <- (map readTrackType) <$> keys tracks
  when (null tracksKeys) $ fail $ ForeignError "TracksMap is empty"
  when (not $ all isJust tracksKeys) $ fail $ ForeignError "TracksMap keys are not all TrackType"

  let tracks' = unsafeCoerce tracks
  pure $ BrowserConfig { wrapRenderer, browser, tracks: tracks' }
