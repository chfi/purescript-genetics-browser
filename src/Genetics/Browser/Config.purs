module Genetics.Browser.Config
       ( BrowserConfig(..)
       , parseBrowserConfig
       ) where

import Prelude

import Control.Monad.Except (catchError, withExcept)
import Data.Array (null)
import Data.Foldable (all)
import Data.Foreign (F, Foreign, ForeignError(..), fail, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.Maybe (isJust)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Genetics.Browser.Biodalliance (BrowserConstructor, RenderWrapper)
import Genetics.Browser.Biodalliance.Config (RendererInfo, parseRenderers)
import Genetics.Browser.Config.Track (TracksMap, readTrackType, readTracksMap)
import Unsafe.Coerce (unsafeCoerce)


-- TODO `wrapRenderer` and `browser` should both be in a BD-specific config,
-- and be optional if there are no BD tracks.
-- | The configuration type for the whole browser
newtype BrowserConfig = BrowserConfig { wrapRenderer :: RenderWrapper
                                      , bdRenderers :: StrMap RendererInfo
                                      , browser :: BrowserConstructor
                                      , tracks :: TracksMap
                                      }

readTaggedWithError :: forall a. String -> String -> Foreign -> F a
readTaggedWithError s e f = withExcept ((<>) (pure $ ForeignError e)) $ unsafeReadTagged s f

parseBrowserConfig :: Foreign -> F BrowserConfig
parseBrowserConfig f = do
  wrapRenderer <- f ! "wrapRenderer" >>= readTaggedWithError "Function" "Error on 'wrapRenderer':"
  browser <- f ! "browser" >>= readTaggedWithError "Function" "Error on 'browser':"
  tracks <- f ! "tracks" >>= readTracksMap
  bdRenderers <- (f ! "renderers" >>= parseRenderers) `catchError` (const $ pure StrMap.empty)
  pure $ BrowserConfig { wrapRenderer, bdRenderers, browser, tracks }
