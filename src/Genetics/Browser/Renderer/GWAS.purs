module Genetics.Browser.Renderer.GWAS
       ( render
       ) where

import Prelude
import Math as Math
import Control.Error.Util (hush)
import Control.Monad.Except (runExcept)
import Data.Foldable (foldr)
import Data.Foreign (F, Foreign, readString, toForeign)
import Data.Foreign.Class (readProp)
import Data.Foreign.Index (prop)
import Data.Foreign.Null (writeNull)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Genetics.Browser.Feature (Feature(Feature), chrToScreen)
import Genetics.Browser.Glyph (Glyph, circle, fill, stroke)
import Genetics.Browser.GlyphF.Interpret (writeGlyph)
import Genetics.Browser.Types (View, Renderer)
import Global (readFloat, infinity)

type GWASRow = (score :: Number)
type GWASFeature = Feature GWASRow


-- TODO: convert pValue to negative log here?
-- or at least add function to do it for us
readGWASFeature :: Foreign -> F GWASFeature
readGWASFeature f = do
  fMin <- readProp "min" f
  fMax <- readProp "max" f
  score <- prop "pValue" f >>= readString <#> readFloat
  pure $ Feature { min: fMin, max: fMax, score: score }


glyphifyFeature :: String -> Number -> GWASFeature -> Glyph Unit
glyphifyFeature col h (Feature f) = do
  stroke col
  fill col
  circle { x: f.min, y: h + 10.0 * (Math.log f.score / Math.log 10.0) } 3.0

featureToForeign :: View -> F GWASFeature -> Foreign
featureToForeign v f =
  let tf = chrToScreen v <$> f
      g = glyphifyFeature "#2222dd" v.height <$> tf
      f' = hush $ runExcept f
  in writeGlyph f' g

writeFeatures :: View -> Array Foreign -> Foreign
writeFeatures v fs = toForeign $ featureToForeign v <<< readGWASFeature <$> fs

quant :: Array GWASFeature -> { min :: Number, max :: Number }
quant = foldr (\(Feature cur) {min, max} ->
                let score' = (Math.log cur.score) / (Math.log 10.0)
                    -- max/min are reversed since the score is negative
                in { min: Math.max score' min
                   , max: Math.min score' max })
             { min: (-infinity), max: infinity}

-- TODO: this could be a function in a generic module
writeResult :: Foreign -> Maybe { min :: Number, max :: Number } -> Foreign
writeResult g q = toForeign { glyphs: g
                            , quant: q'
                            }
                  where q' = case q of Just x  -> toForeign x
                                       Nothing -> writeNull

-- TODO right now all other data in the feature is thrown away in the parsing...
-- we should send the original Foreign value as the feature!
render :: Renderer
render v fs =
  let fs' = readGWASFeature <$> fs
      gs = toForeign $ featureToForeign v <$> fs'
      q = quant <$> (sequence $ (hush <<< runExcept) <$> fs')
  in writeResult gs q
