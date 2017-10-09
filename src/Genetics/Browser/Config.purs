module Genetics.Browser.Config
       ( BrowserConfig(..)
       , parseBrowserConfig
       ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (except, withExcept)
import Data.Argonaut (Json)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foreign (F, Foreign, ForeignError(..), readArray, readString, unsafeReadTagged)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(Just))
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Genetics.Browser.Biodalliance (BrowserConstructor, RenderWrapper)
import Genetics.Browser.Biodalliance.Config (RendererInfo, parseRenderers)
import Genetics.Browser.Biodalliance.Types (Biodalliance)
import Genetics.Browser.Config.Track (TracksMap, readTracksMap)
import Genetics.Browser.Cytoscape.Types (Cytoscape)
import Genetics.Browser.Events.TrackSink (SinkConfig)
import Genetics.Browser.Events.TrackSource (SourceConfig, validateSourceConfig)
import Unsafe.Coerce (unsafeCoerce)


-- TODO `wrapRenderer` and `browser` should both be in a BD-specific config,
-- and be optional if there are no BD tracks.
-- | The configuration type for the whole browser
newtype BrowserConfig eb ec = BrowserConfig { wrapRenderer :: RenderWrapper
                                      , bdRenderers :: StrMap RendererInfo
                                      , browser :: BrowserConstructor
                                      , tracks :: TracksMap
                                      , events :: Maybe { bdEventSources :: Array SourceConfig
                                                        , cyEventSources :: Array SourceConfig
                                                        , bdEventSinks :: Array (SinkConfig
                                                                          (Biodalliance -> Eff eb Unit))
                                                        , cyEventSinks :: Array (SinkConfig
                                                                          (Cytoscape -> Eff ec Unit))
                                                        }
                                      }

readJson :: Foreign -> F Json
readJson f = pure $ unsafeCoerce f

readTaggedWithError :: forall a. String -> String -> Foreign -> F a
readTaggedWithError s e f = withExcept (append (pure $ ForeignError e)) $ unsafeReadTagged s f

parseSourceConfig :: Foreign -> F SourceConfig
parseSourceConfig f = do
  eventName     <- f ! "eventName" >>= readString
  eventTemplate <- f ! "eventTemplate" >>= readJson
  rawTemplate   <- f ! "rawTemplate" >>= readJson
  pure $ { eventName
         , eventTemplate
         , rawTemplate
         }

parseSinkConfig :: forall a. Foreign -> F (SinkConfig a)
parseSinkConfig f = do
  eventName <- f ! "eventName" >>= readString
  eventFun <- f ! "eventFun" >>=
              readTaggedWithError "Function" ("Error on sink config for event: " <> eventName)
  pure $ { eventName, eventFun }


eitherToF :: Either String ~> F
eitherToF = except <<< lmap (pure <<< ForeignError)

parseBrowserConfig :: forall eb ec. Foreign -> F (BrowserConfig eb ec)
parseBrowserConfig f = do
  wrapRenderer <- f ! "wrapRenderer" >>= readTaggedWithError "Function" "Error on 'wrapRenderer':"
  browser <- f ! "browser" >>= readTaggedWithError "Function" "Error on 'browser':"
  tracks <- f ! "tracks" >>= readTracksMap

  events <- do
    sources <- f ! "eventSources"
    bdSources <- sources ! "bd" >>= readArray >>= traverse parseSourceConfig
    cySources <- sources ! "cy" >>= readArray >>= traverse parseSourceConfig

    _ <- eitherToF $ traverse validateSourceConfig bdSources
    _ <- eitherToF $ traverse validateSourceConfig cySources

    sinks <- f ! "eventSinks"
    bdSinks <- sinks ! "bd" >>= readArray >>= traverse parseSinkConfig
    cySinks <- sinks ! "cy" >>= readArray >>= traverse parseSinkConfig
    

    pure $ Just $ { bdEventSources: bdSources
                  , cyEventSources: cySources
                  , bdEventSinks: bdSinks
                  , cyEventSinks: cySinks
                  }

  bdRenderers <- f ! "renderers" >>= parseRenderers
  pure $ BrowserConfig { wrapRenderer, bdRenderers, browser, tracks, events }
