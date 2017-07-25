module Test.Glyph
       ( spec
       , svgCanvasTest
       ) where

import Prelude
import DOM.Node.Types as DOM
import Genetics.Browser.GlyphF.Canvas as Canvas
import Genetics.Browser.GlyphF.SVG as SVG
import Test.QuickCheck.Laws.Data as Data
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import DOM (DOM)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (traverse_)
import Genetics.Browser.Feature (Feature(..), ScreenFeature, featureToScreen)
import Genetics.Browser.Glyph (Glyph, circle, fill, rect, stroke)
import Genetics.Browser.GlyphPosition (GlyphPosition)
import Genetics.Browser.Units (Bp(..), MBp(..))
import Graphics.Canvas (getCanvasElementById, getContext2D, translate)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy(..))


-- QuickCheck & unit tests
spec :: ∀ eff. Spec _ Unit
spec = do
  let prxGlyph = Proxy :: Proxy GlyphPosition
  describe "GlyphPosition instances" do
    it "is a Semigroup" $ do
      liftEff $ Data.checkSemigroup prxGlyph
    it "is a Monoid" $ do
      liftEff $ Data.checkMonoid prxGlyph


-- In browser SVG/Canvas tests
foreign import addElementToDiv :: ∀ eff. String -> DOM.Element -> Eff ( dom :: DOM | eff ) Unit
foreign import setOnLoad :: ∀ eff. Eff eff Unit -> Eff eff Unit

exGlyph :: Glyph Unit
exGlyph = do
  stroke "#ff0000"
  fill "#333333"
  rect {x: 0.0, y: 0.0} {x: 20.0, y: 140.0}
  stroke "#00ff00"
  fill "#777777"
  circle {x: 48.0, y: 30.0} 5.356
  stroke "#0000ff"
  fill "#aaaaaa"
  rect {x: 0.0, y: 10.0} {x: 400.0, y: 15.0}


exFeature1 :: Feature MBp Unit
exFeature1 = Feature "1" (MBp (-5.0)) (MBp 5.0) unit
exFeature2 :: Feature MBp Unit
exFeature2 = Feature "1" (MBp 10.0) (MBp 20.0) unit
exFeature3 :: Feature Bp Unit
exFeature3 = Feature "1" (Bp 60000.0) (Bp 61000.0) unit


glyph1 :: Glyph Unit
glyph1 = glyphify 0.0 (featureToScreen (MBp 0.0) (MBp 5.0) exFeature1)

glyph2 :: Glyph Unit
glyph2 = glyphify 50.0 (featureToScreen (MBp 0.0) (MBp 5.0) exFeature2)

glyph3 :: Glyph Unit
glyph3 = glyphify 100.0 (featureToScreen (Bp 0.0) (Bp 5.0) exFeature3)


glyphify :: Number -> ScreenFeature Unit -> Glyph Unit
glyphify y (Feature _ xl xr _) = do
  stroke "#ff0000"
  fill "#555555"
  rect { x: xl, y: y } { x: xr, y: y + 40.0 }


runBrowserTest :: Eff _ Unit
runBrowserTest = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> log "couldn't find canvas"
    Just c  -> do
      ctx <- getContext2D c
      _ <- translate {translateX: -200.0, translateY: 0.0} ctx
      log "rendering glyph to canvas"
      traverse_ (Canvas.renderGlyph ctx) [glyph1, glyph2, glyph3]

  log "rendering glyph to svg"
  traverse_ (\g -> SVG.renderGlyph g >>= addElementToDiv "svgDiv") [glyph1, glyph2, glyph3]
  pure unit

svgCanvasTest :: Eff _ Unit
svgCanvasTest = setOnLoad runBrowserTest
