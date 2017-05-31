module Genetics.Browser.GlyphF.Interpret
       ( writeGlyph )
       where

import Prelude
import Genetics.Browser.GlyphF.Canvas as Canvas
import Genetics.Browser.GlyphF.SVG as SVG
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Foreign (F, Foreign, renderForeignError, toForeign)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Genetics.Browser.Feature (Feature(..))
import Genetics.Browser.Glyph (Glyph)
import Genetics.Browser.GlyphF.Position (glyphToGlyphPosition)

-- TODO: this should be dealt with in Feature, in a safer way
helper :: ∀ c r.
  Feature c r
  -> { chr :: String
     , min :: c
     , max :: c
     }
helper (Feature chr xl xr _) = {chr: chr, min: xl, max: xr}

writeGlyph' :: ∀ a c r. Maybe (Feature c r) -> Glyph a -> Foreign
writeGlyph' f g = toForeign { "draw": unsafePerformEff <<< \ctx -> Canvas.renderGlyph ctx g
                            , "min": const p.min
                            , "max": const p.max
                            , "minY": const p.minY
                            , "maxY": const p.maxY
                            , "feature": f'
                            , "toSVG": unsafePerformEff <<< \_ -> SVG.renderGlyph g
                            }
    where p = unwrap $ glyphToGlyphPosition g
          f' = toNullable $ (map helper) f


writeGlyph :: ∀ a c r. Maybe (Feature c r) -> F (Glyph a) -> Foreign
writeGlyph f fG = case runExcept fG of
  Left errors -> toForeign $ fold $ renderForeignError <$> errors
  Right glyph -> writeGlyph' f glyph
