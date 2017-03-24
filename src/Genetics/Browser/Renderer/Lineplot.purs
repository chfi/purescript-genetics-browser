module Genetics.Browser.Renderer.Lineplot
       where

import Prelude
import Control.Error.Util (hush)
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Foreign.Class (readProp)
import Data.Foreign.Index (prop)
import Data.Foreign.Null (writeNull)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Genetics.Browser.Feature (Feature(Feature), chrToScreen)
import Genetics.Browser.Glyph (Glyph, fill, path, stroke)
import Genetics.Browser.GlyphF.Interpret (writeGlyph)
import Genetics.Browser.Types (Renderer)
import Global (readFloat)

type LineRow = (score :: Number)
type LineFeature = Feature LineRow

readLineFeature :: Foreign -> F LineFeature
readLineFeature f = do
  fMin <- readProp "min" f
  fMax <- readProp "max" f
  score <- prop "score" f >>= readString <#> readFloat
  pure $ Feature { min: fMin, max: fMax, score: score }


-- TODO: error out on invalid configurations (parse using Foreign?)
type LinePlotConfig =
  { minScore :: Number
  , maxScore :: Number
  , color :: String
  }

normalizeScore :: LinePlotConfig -> Number -> Number -> Number
normalizeScore conf h y = ((y - conf.minScore) / (conf.maxScore - conf.minScore)) * h


linePlot :: LinePlotConfig -> Number -> Array LineFeature -> Glyph Unit
linePlot cfg h fs = do
  stroke cfg.color
  let ps = (\(Feature f) -> { x: f.min, y: h - (normalizeScore cfg h f.score) } ) <$> fs
  path ps


writeResult :: Foreign -> Maybe { min :: Number, max :: Number } -> Foreign
writeResult g q = toForeign { glyphs: [g]
                            , quant: q'
                            }
                  where q' = case q of Just x  -> toForeign x
                                       Nothing -> writeNull





render :: LinePlotConfig -> Renderer
render cfg v fs =
  let fs' = sequence $ map (chrToScreen v) <$> readLineFeature <$> fs
      g = linePlot cfg v.height <$> fs'
  in writeResult
      (writeGlyph Nothing g)
      (Just { min: cfg.minScore, max: cfg.maxScore })
