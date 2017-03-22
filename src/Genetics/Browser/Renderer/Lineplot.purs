module Genetics.Browser.Renderer.Lineplot
       where

import Prelude

import Genetics.Browser.Feature (Feature(Feature))
import Genetics.Browser.Glyph (Glyph, path, stroke)

type LineRow = (score :: Number)
type LineFeature = Feature LineRow


type LinePlotConfig =
  { minScore :: Number
  , maxScore :: Number
  , color :: String
  }

normalizeScore :: LinePlotConfig -> Number -> Number
normalizeScore conf y = ((y - conf.minScore) / (conf.maxScore))

type View = { viewStart :: Number
            , scale :: Number
            }


linePlot :: Array LineFeature -> Glyph Unit
linePlot fs = do
  stroke "#ff0000"
  let ps = (\(Feature f) -> { x: f.min, y: f.max } ) <$> fs
  path ps

glyphifyFeatures :: View -> Array LineFeature -> Glyph Unit
glyphifyFeatures v fs = linePlot fs



qtlPlotConfig :: LinePlotConfig
qtlPlotConfig = { minScore: 3.0
                , maxScore: 5.0
                , color: "#dd0000"
                }
