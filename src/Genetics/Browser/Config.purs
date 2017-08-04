module Genetics.Browser.Config
       ( BrowserConfig(..)
       , parseBrowserConfig
       ) where

import Prelude
import Control.Monad.Except (withExcept)
import Data.Array (null)
import Data.Foldable (all)
import Data.Foreign (F, Foreign, ForeignError(..), fail, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.Maybe (isJust)
import Genetics.Browser.Biodalliance (BrowserConstructor, RenderWrapper)
import Genetics.Browser.Config.Track (TracksMap, readTrackType)
import Unsafe.Coerce (unsafeCoerce)


-- TODO `wrapRenderer` and `browser` should both be in a BD-specific config,
-- and be optional if there are no BD tracks.
-- | The configuration type for the whole browser
newtype BrowserConfig = BrowserConfig { wrapRenderer :: RenderWrapper
                                      , browser :: BrowserConstructor
                                      , tracks :: TracksMap
                                      }

readTaggedWithError :: forall a. String -> String -> Foreign -> F a
readTaggedWithError s e f = withExcept ((<>) (pure $ ForeignError e)) $ unsafeReadTagged s f

parseBrowserConfig :: Foreign -> F BrowserConfig
parseBrowserConfig f = do
  wrapRenderer <- f ! "wrapRenderer" >>= readTaggedWithError "Function" "Error on 'wrapRenderer':"
  browser <- f ! "browser" >>= readTaggedWithError "Function" "Error on 'browser':"
  tracks <- f ! "tracks"
  tracksKeys <- (map readTrackType) <$> keys tracks
  when (null tracksKeys) $ fail $ ForeignError "TracksMap is empty"
  when (not $ all isJust tracksKeys) $ fail $ ForeignError "TracksMap keys are not all TrackType"
  let tracks' = unsafeCoerce tracks
  pure $ BrowserConfig { wrapRenderer, browser, tracks: tracks' }
