module Genetics.Browser.Events
       ( Location(..)
       , Range(..)
       , Score(..)
       )
       where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Newtype (class Newtype, unwrap, wrap)
import Genetics.Browser.Units (Bp, Chr)


newtype Location = Location { chr :: Chr
                            , pos :: Bp
                            }

derive instance newtypeLocation :: Newtype Location _

instance encodeJSONLocation :: EncodeJson Location where
  encodeJson (Location {chr, pos})
    = "chr" := (unwrap chr)
   ~> "pos" := (unwrap pos)
   ~> jsonEmptyObject

instance decodeJSONLocation :: DecodeJson Location where
  decodeJson json = do
    obj <- decodeJson json
    chr <- wrap <$> obj .? "chr"
    pos <- wrap <$> obj .? "pos"
    pure $ Location { chr, pos }


newtype Range = Range { chr :: Chr
                      , minPos :: Bp
                      , maxPos :: Bp
                      }

derive instance newtypeRange :: Newtype Range _


instance encodeJSONRange :: EncodeJson Range where
  encodeJson (Range {chr, minPos, maxPos})
    = "chr" := (unwrap chr)
   ~> "minPos" := (unwrap minPos)
   ~> "maxPos" := (unwrap maxPos)
   ~> jsonEmptyObject

instance decodeJSONRange :: DecodeJson Range where
  decodeJson json = do
    obj <- decodeJson json
    chr <- wrap <$> obj .? "chr"
    minPos <- wrap <$> obj .? "minPos"
    maxPos <- wrap <$> obj .? "maxPos"
    pure $ Range { chr, minPos, maxPos }


newtype Score = Score { score :: Number }

derive instance newtypeScore :: Newtype Score _

instance encodeJSONScore :: EncodeJson Score where
  encodeJson (Score {score})
    = "score" := score
   ~> jsonEmptyObject

instance decodeJSONScore :: DecodeJson Score where
  decodeJson json = do
    obj <- decodeJson json
    score <- obj .? "score"
    pure $ Score { score }
