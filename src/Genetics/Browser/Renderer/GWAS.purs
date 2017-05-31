module Genetics.Browser.Renderer.GWAS
       ( render
       ) where

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Foreign (F, Foreign, readNumber, readString, toForeign)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Traversable (sequence)
import Genetics.Browser.Feature (Feature(Feature), ScreenFeature, featureToScreen)
import Genetics.Browser.Glyph (Glyph, circle, fill, stroke)
import Genetics.Browser.GlyphF.Interpret (writeGlyph)
import Genetics.Browser.Types (View, Renderer)
import Genetics.Browser.Units (Bp(Bp))
import Global (readFloat, infinity)
import Math as Math
import Prelude

type GWASData = {score :: Number}
type GWASFeature = Feature Bp GWASData
type GWASScreenFeature = ScreenFeature GWASData


-- TODO: convert pValue to negative log here?
-- or at least add function to do it for us
readGWASFeature :: String -> Foreign -> F GWASFeature
readGWASFeature chr f = do
  fMin <- Bp <$> (readProp "min" f >>= readNumber)
  fMax <- Bp <$> (readProp "max" f >>= readNumber)
  score <- readProp "pValue" f >>= readString <#> readFloat
  pure $ Feature chr fMin fMax { score }


  -- TODO there are problems with having a direct Feature -> Glyph translation here...
  -- in this case we _do_ need to have screen coordinates as HCoordinates. hm.
  -- maybe we can do something fugly to fix it.
glyphifyFeature :: String -> Number -> GWASScreenFeature -> Glyph Unit
glyphifyFeature col h (Feature chr xl xr r) = do
  stroke col
  fill col
  circle { x: xl, y: h + 10.0 * (Math.log r.score / Math.log 10.0) } 3.0


hush :: ∀ a b. Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right b) = Just b

featureToForeign :: View -> F GWASFeature -> Foreign
featureToForeign v f =
  let tf = featureToScreen (Bp v.viewStart) (Bp v.scale) <$> f
      g = glyphifyFeature "#2222dd" v.height <$> tf
      f' = hush $ runExcept f
  in writeGlyph f' g

quant :: Array GWASFeature -> { min :: Number, max :: Number }
                                     -- ugly :|
quant = foldr (\(Feature _ _ _ r) {min, max} ->
                let score' = (Math.log r.score) / (Math.log 10.0)
                    -- max/min are reversed since the score is negative
                in { min: Math.max score' min
                   , max: Math.min score' max })
             { min: (-infinity), max: infinity}

-- TODO: this could be a function in a generic module
writeResult :: Foreign -> Maybe { min :: Number, max :: Number } -> Foreign
writeResult g q = toForeign { glyphs: g
                            , quant: q'
                            }
                  where q' = toForeign $ toNullable q
                  -- where q' = case q of Just x  -> toForeign x
                  --                      Nothing -> writeNull

-- TODO right now all other data in the feature is thrown away in the parsing...
-- we should send the original Foreign value as the feature!
render :: Renderer
render v fs =
  let fs' = readGWASFeature v.chr <$> fs
      gs = toForeign $ featureToForeign v <$> fs'
      q = quant <$> (sequence $ (hush <<< runExcept) <$> fs')
  in writeResult gs q
