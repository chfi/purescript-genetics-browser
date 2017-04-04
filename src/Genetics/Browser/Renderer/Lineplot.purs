module Genetics.Browser.Renderer.Lineplot
       where

import Prelude
import Control.Alt ((<|>))
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Foreign.Class (readProp)
import Data.Foreign.Index (prop)
import Data.Foreign.Null (writeNull)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Genetics.Browser.Feature (Feature, ScreenFeature, feature, featureToScreen, withFeature)
import Genetics.Browser.Glyph (Glyph, path, stroke)
import Genetics.Browser.GlyphF.Interpret (writeGlyph)
import Genetics.Browser.Types (Renderer)
import Genetics.Browser.Units (Bp(..))
import Global (readFloat)

type LineRow = (score :: Number)
type LineFeature = Feature Bp LineRow
type LineScreenFeature = ScreenFeature LineRow

  -- TODO: refactor into separate module, handle both string encoded regular numbers as well as string encoded exponential numbers, _and_ regular numbers
readLineFeature :: String -> Foreign -> F LineFeature
readLineFeature chr f = do
  fMin <- Bp <$> readProp "min" f
  fMax <- Bp <$> readProp "max" f
  score <- (readProp "score" f) <|>
           (prop "score" f >>= readString <#> readFloat)
  pure $ feature { chr: chr, min: fMin, max: fMax, score: score }


-- TODO: error out on invalid configurations (parse using Foreign?)
type LinePlotConfig =
  { minScore :: Number
  , maxScore :: Number
  , color :: String
  }

normalizeScore :: LinePlotConfig -> Number -> Number -> Number
normalizeScore conf h y = ((y - conf.minScore) / (conf.maxScore - conf.minScore)) * h


linePlot :: LinePlotConfig -> Number -> Array LineScreenFeature -> Glyph Unit
linePlot cfg h fs = do
  stroke cfg.color
  let ps = withFeature (\f -> { x: f.min, y: h - (normalizeScore cfg h f.score) } ) <$> fs
  path ps


writeResult :: Foreign -> Maybe { min :: Number, max :: Number } -> Foreign
writeResult g q = toForeign { glyphs: [g]
                            , quant: q'
                            }
                  where q' = case q of Just x  -> toForeign x
                                       Nothing -> writeNull


render :: LinePlotConfig -> Renderer
render cfg v fs =
  let fs' :: _
      fs' = sequence $ map (featureToScreen (Bp v.viewStart) (Bp v.scale)) <$>
            (readLineFeature v.chr <$> fs)
      g = linePlot cfg v.height <$> fs'
  in writeResult
      (writeGlyph Nothing g)
      (Just { min: cfg.minScore, max: cfg.maxScore })
