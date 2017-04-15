module Genetics.Browser.Renderer.Renderer
       where

import Prelude
import Math as Math
import Data.Foldable (foldr)
import Data.Foreign (Foreign, F)
import Data.Profunctor (class Profunctor)
import Genetics.Browser.Feature (Feature(..), ScreenFeature, featureToScreen)
import Genetics.Browser.Glyph (Glyph, circle, fill, stroke)
import Genetics.Browser.Types (View, Quant)
import Genetics.Browser.Units (class HCoordinate)
import Global (infinity)


-- a Track r a b can parse a's into r's, and then r's into b's.
-- however... this doesn't quite cover what a track does.
-- a track /fetches/ and parses. Meaning the first argument to the data constructor
-- should really be an Eff (??) r ... or an Aff?
-- so I suppose that means it should be any contravariant functor?
-- is Eff contravariant? I doubt it.
-- indeed, it doesn't make much sense for this to be structured like it is.
-- we want something that produces r's. That's it.
-- so it's another bifunctor, really!
data Track r a b = Track (a -> r) (r -> b)

data Track' r f b = Track' (f r) (r -> b)
-- then we need to use a smart constructor?

type BDTrack r = Track r Foreign (Glyph Unit)

instance functorTrack :: Functor (Track r a) where
  map f (Track p r) = Track p (f <$> r)

instance profunctorTrack :: Profunctor (Track r) where
  dimap f g (Track parse render) = Track (parse <<< f) (render >>> g)



-- rather, it's something that can fetch from a and render to a track.
-- we don't even need a b - rather, the b doesn't define the output, not exactly.
-- it could rather define the features kept in it.
-- what would the a be? a fetcher?
-- an URI?
-- an (a -> b)?


-- renderTrack :: ∀ a b. Track a b -> Foreign
-- renderTrack = ?r

-- first we parse the feature
-- readFeature :: ∀ c r. Foreign -> F (Feature c r)
-- readFeature = ?r


-- glyphifyFeature :: ∀ conf r. conf -> ScreenFeature r -> Glyph Unit
-- glyphifyFeature = ?g



type Glyphifier conf c r = conf -> Feature c r -> Glyph Unit
-- glyphify :: ∀ c r. Feature c r -> Glyph Unit
-- glyphify = ?g

-- glyphify after transforming into canvas coordinate s
-- glyphifyGWAS :: (Number -> Number) -> Glyphifier { col :: String } Number (score :: Number)
glyphifyGWAS :: _
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
