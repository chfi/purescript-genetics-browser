module Genetics.Browser.Biodalliance.Config where

import Prelude

import Data.Foreign (F, Foreign, readInt)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.Options (Option, Options, opt, options)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Genetics.Browser.Biodalliance.Types (BD, Biodalliance, Renderer)
import Genetics.Browser.Config.Track (BDTrackConfig)
import Genetics.Browser.Types (class HCoordinate, Bp, Chr, bp)
import Unsafe.Coerce (unsafeCoerce)


sources :: Option Biodalliance (Array BDTrackConfig)
sources = opt "sources"

-- Renderers need some extra info for WrappedRenderer to be able to do its thing
type RendererInfo = { renderer :: Renderer, canvasHeight :: Int }

type BDRenderers = StrMap RendererInfo

renderers :: Option Biodalliance BDRenderers
renderers = opt "renderers"

maxHeight :: Option Biodalliance Int
maxHeight = opt "maxHeight"


parseRendererInfo :: Foreign -> String -> F (Tuple String RendererInfo)
parseRendererInfo f name = do
  obj <- f ! name
  renderer <- unsafeCoerce <$> obj ! "renderer"
  canvasHeight <- obj ! "canvasHeight" >>= readInt
  pure $ Tuple name { renderer, canvasHeight }


parseRenderers :: Foreign -> F BDRenderers
parseRenderers f = do
  names <- keys f
  tuples <- traverse (parseRendererInfo f) names
  pure $ StrMap.fromFoldable tuples
