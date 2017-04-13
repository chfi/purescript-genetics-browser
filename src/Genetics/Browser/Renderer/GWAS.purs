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
import Genetics.Browser.Feature (Feature, ScreenFeature, feature, featureToScreen, withFeature, withFeature')
import Genetics.Browser.Glyph (Glyph, circle, fill, stroke)
import Genetics.Browser.GlyphF.Interpret (writeGlyph)
import Genetics.Browser.Types (View, Renderer)
import Genetics.Browser.Units (Bp(Bp))
import Global (readFloat, infinity)

type GWASRow = (score :: Number)
type GWASFeature = Feature Bp GWASRow
type GWASScreenFeature = ScreenFeature GWASRow


-- TODO: convert pValue to negative log here?
-- or at least add function to do it for us
readGWASFeature :: String -> Foreign -> F GWASFeature
readGWASFeature chr f = do
  fMin <- Bp <$> readProp "min" f
  fMax <- Bp <$> readProp "max" f
  score <- prop "pValue" f >>= readString <#> readFloat
  pure $ feature { chr: chr, min: fMin, max: fMax, score: score }


  -- TODO there are problems with having a direct Feature -> Glyph translation here...
  -- in this case we _do_ need to have screen coordinates as HCoordinates. hm.
  -- maybe we can do something fugly to fix it.
glyphifyFeature :: String -> Number -> GWASScreenFeature -> Glyph Unit
glyphifyFeature col h = withFeature $ \f -> do
  stroke col
  fill col
  circle { x: f.min, y: h + 10.0 * (Math.log f.score / Math.log 10.0) } 3.0

featureToForeign :: View -> F GWASFeature -> Foreign
featureToForeign v f =
  let tf = featureToScreen (Bp v.viewStart) (Bp v.scale) <$> f
      g = glyphifyFeature "#2222dd" v.height <$> tf
      f' = hush $ runExcept f
  in writeGlyph f' g

quant :: Array GWASFeature -> { min :: Number, max :: Number }
                                     -- ugly :|
quant = foldr (\cur {min, max} -> withFeature' cur $ \cur' ->
                let score' = (Math.log cur'.score) / (Math.log 10.0)
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
  let fs' = readGWASFeature v.chr <$> fs
      gs = toForeign $ featureToForeign v <$> fs'
      q = quant <$> (sequence $ (hush <<< runExcept) <$> fs')
  in writeResult gs q
