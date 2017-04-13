module Genetics.Browser.Renderer.Renderer
       where

import Prelude
import Math as Math
import Data.Foldable (foldr)
import Data.Foreign (Foreign, F)
import Genetics.Browser.Feature (Feature(..), ScreenFeature, featureToScreen)
import Genetics.Browser.Glyph (Glyph, circle, fill, stroke)
import Genetics.Browser.Types (View, Quant)
import Genetics.Browser.Units (class HCoordinate)
import Global (infinity)



-- a Track is something that makes a's into b's,
-- where the a's may be fetched remotely, and the b's are something that can be Glyphified.
-- data Track a b = Track a b

-- rather, it's something that can fetch from a and render to a track.
-- we don't even need a b - rather, the b doesn't define the output, not exactly.
-- it could rather define the features kept in it.
-- what would the a be? a fetcher?
-- an URI?
-- an (a -> b)?


-- renderTrack :: ∀ a b. Track a b -> Foreign
-- renderTrack = ?r

-- first we parse the feature
readFeature :: ∀ c r. Foreign -> F (Feature c r)
readFeature = ?r


glyphifyFeature :: ∀ conf r. conf -> ScreenFeature r -> Glyph Unit
glyphifyFeature = ?g



type Glyphifier conf c r = conf -> Feature c r -> Glyph Unit
-- glyphify :: ∀ c r. Feature c r -> Glyph Unit
-- glyphify = ?g

-- glyphify after transforming into canvas coordinates
glyphifyGWAS :: (Number -> Number) -> Glyphifier { col :: String } Number (score :: Number)
glyphifyGWAS f {col} (Feature chr xl _ r) = do
  stroke col
  fill col
  circle { x: xl, y: f r.score } 3.0




-- this is basically just (writeGlyph <<< glyphifyFeature <<< toScreen)
-- though we need to send some configuration around.
featureToForeign :: ∀ c r. View -> Feature c r -> Foreign
featureToForeign = ?f2f


-- this isn't quite enough - it needs a way to go from Feature c r to a score,
-- possibly with a config, much like glyphifyFeature has.
-- should there be a middle step where we get that data?
-- I suppose Features could be categorised into (at least) quant and non-quant Features.
  -- GADTs would be nice for that!
quant :: ∀ c r. (r -> Number) -> Array (Feature c r) -> Quant
quant f = foldr (\(Feature _ _ _ r) {min, max} -> let y = f r in
                  { min: Math.max y min
                  , max: Math.min y max
                  }) { min: (-infinity), max: infinity }



-- render :: View -> Array ForeignFeature -> ForeignGlyph
