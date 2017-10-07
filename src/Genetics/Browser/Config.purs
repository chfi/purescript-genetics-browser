module Genetics.Browser.Config
       ( BrowserConfig(..)
       , parseBrowserConfig
       ) where

import Prelude

import Control.Monad.Except (except, throwError, withExcept)
import Data.Argonaut (Json)
import Data.Bifunctor (lmap)
import Data.Either (isLeft, Either(..))
import Data.Foldable (any)
import Data.Foreign (F, Foreign, ForeignError(..), readArray, readString, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..), isNothing)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Genetics.Browser.Biodalliance (BrowserConstructor, RenderWrapper)
import Genetics.Browser.Biodalliance.Config (RendererInfo, parseRenderers)
import Genetics.Browser.Config.Track (TracksMap, readTracksMap)
import Genetics.Browser.Events.TrackSource (SourceConfig, makeTrackSource, validateSourceConfig)
import Unsafe.Coerce (unsafeCoerce)


-- TODO `wrapRenderer` and `browser` should both be in a BD-specific config,
-- and be optional if there are no BD tracks.
-- | The configuration type for the whole browser
newtype BrowserConfig = BrowserConfig { wrapRenderer :: RenderWrapper
                                      , bdRenderers :: StrMap RendererInfo
                                      , browser :: BrowserConstructor
                                      , tracks :: TracksMap
                                      , events :: Maybe { bdEventSources :: Array SourceConfig
                                                        , cyEventSources :: Array SourceConfig
                                                        }
                                      }

readJson :: Foreign -> F Json
readJson f = pure $ unsafeCoerce f

parseSourceConfig :: Foreign -> F SourceConfig
parseSourceConfig f = do
  eventName     <- f ! "eventName" >>= readString
  eventTemplate <- f ! "eventTemplate" >>= readJson
  rawTemplate   <- f ! "rawTemplate" >>= readJson
  pure $ { eventName
         , eventTemplate
         , rawTemplate
         }

readTaggedWithError :: forall a. String -> String -> Foreign -> F a
readTaggedWithError s e f = withExcept (append (pure $ ForeignError e)) $ unsafeReadTagged s f

eitherToF :: Either String ~> F
eitherToF = except <<< lmap (pure <<< ForeignError)

parseBrowserConfig :: Foreign -> F BrowserConfig
parseBrowserConfig f = do
  wrapRenderer <- f ! "wrapRenderer" >>= readTaggedWithError "Function" "Error on 'wrapRenderer':"
  browser <- f ! "browser" >>= readTaggedWithError "Function" "Error on 'browser':"
  tracks <- f ! "tracks" >>= readTracksMap

  events <- do
    evs <- f ! "eventSources"
    bd <- evs ! "bd" >>= readArray >>= traverse parseSourceConfig
    cy <- evs ! "cy" >>= readArray >>= traverse parseSourceConfig

    _ <- eitherToF $ traverse validateSourceConfig bd
    _ <- eitherToF $ traverse validateSourceConfig cy

    pure $ Just $ { bdEventSources: bd
                  , cyEventSources: cy
                  }

  bdRenderers <- f ! "renderers" >>= parseRenderers
  pure $ BrowserConfig { wrapRenderer, bdRenderers, browser, tracks, events }
