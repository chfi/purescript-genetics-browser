module Genetics.Browser.Renderer.Lineplot
       where

import Prelude
import Control.Alt ((<|>))
import Data.Foreign (F, Foreign, readNumber, readString, toForeign)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Traversable (sequence)
import Genetics.Browser.Feature (Feature(..), ScreenFeature, feature, featureToScreen)
import Genetics.Browser.Glyph (Glyph, path, stroke)
import Genetics.Browser.GlyphF.Interpret (writeGlyph)
import Genetics.Browser.Types (Renderer)
import Genetics.Browser.Units (Bp(..))
import Global (readFloat)

type LineData = { score :: Number }
type LineFeature = Feature Bp LineData
type LineScreenFeature = ScreenFeature LineData

  -- TODO: refactor into separate module, handle both string encoded regular numbers as well as string encoded exponential numbers, _and_ regular numbers
readLineFeature :: String -> Foreign -> F LineFeature
readLineFeature chr f = do
  fMin <- Bp <$> (readProp "min" f >>= readNumber)
  fMax <- Bp <$> (readProp "max" f >>= readNumber)
  -- score <- readProp "score" f >>= (\(x :: _) -> x <|> readString x <#> readFloat)
  score <- (readProp "score" f >>= readNumber)
           -- (readProp "score" f >>= readString <#> readFloat)
  pure $ Feature chr fMin fMax { score }


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
  let ps = (\(Feature _ xl xr r) -> { x: xl, y: h - (normalizeScore cfg h r.score) } ) <$> fs
  path ps


writeResult :: Foreign -> Maybe { min :: Number, max :: Number } -> Foreign
writeResult g q = toForeign { glyphs: [g]
                            , quant: q'
                            }
                  where q' = toForeign $ toNullable q
                  -- where q' = case q of Just x  -> toForeign x
                  --                      Nothing -> toNullable Nothing


render :: LinePlotConfig -> Renderer
render cfg v fs =
  let fs' :: _
      fs' = sequence $ map (featureToScreen (Bp v.viewStart) (Bp v.scale)) <$>
            (readLineFeature v.chr <$> fs)
      g = linePlot cfg v.height <$> fs'
  in writeResult
      (writeGlyph Nothing g)
      (Just { min: cfg.minScore, max: cfg.maxScore })
