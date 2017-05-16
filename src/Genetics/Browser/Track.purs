module Genetics.Browser.Track
       where

import Prelude
import Math as Math
import Control.Monad.Eff (Eff)
import Data.Bifunctor (class Bifunctor)
import Data.Foldable (foldr)
import Data.Foreign (Foreign, F)
import Data.Lens.Iso (iso)
import Data.Lens.Types (Iso')
import Data.Profunctor (class Profunctor)
import Genetics.Browser.Feature (Feature(..), ScreenFeature, featureToScreen)
import Genetics.Browser.Glyph (Glyph, circle, fill, stroke)
import Genetics.Browser.Types (View, Quant)
import Genetics.Browser.Units (class HCoordinate)
import Global (infinity)
import Unsafe.Coerce (unsafeCoerce)


newtype BDFeature = BDFeature Foreign


-- don't think this can be a functor. Feature c r is both covariant and contravariant...
-- well, this one absolutely cannot be a functor.
-- data Track eff c r = Track
--                      (GenRange c -> Eff eff (Feature c r))
--                      (Iso' (Feature c r) BDFeature)
--                      (Feature c r -> Array (Glyph Unit))

-- Would be nice with some way to tag the `b` value as something that gets serialized. idk
-- same way with `f` being a functor. oh well
-- i guess it's an isomorphism sort of. eh.
data Track2 f a b = Track2
                    (f a)
                    (Iso' a b)
                    (a -> Glyph Unit)




type Glyphifier conf c r = conf -> Feature c r -> Glyph Unit
-- glyphify :: ∀ c r. Feature c r -> Glyph Unit
-- glyphify = ?g

-- glyphify after transforming into canvas coordinate s
glyphifyGWAS :: (Number -> Number) -> Glyphifier { col :: String } Number {score :: Number}
glyphifyGWAS f {col} (Feature chr xl _ r) = do
  stroke col
  fill col
  circle { x: xl, y: f r.score } 3.0


-- this is basically just (writeGlyph <<< glyphifyFeature <<< toScreen)
-- though we need to send some configuration around.
-- featureToForeign :: ∀ c r. View -> Feature c r -> Foreign
-- featureToForeign = ?f2f


quant :: ∀ c r. (r -> Number) -> Array (Feature c r) -> Quant
quant f = foldr (\(Feature _ _ _ r) {min, max} -> let y = f r in
                  { min: Math.max y min
                  , max: Math.min y max
                  }) { min: (-infinity), max: infinity }



-- render :: View -> Array ForeignFeature -> ForeignGlyph
