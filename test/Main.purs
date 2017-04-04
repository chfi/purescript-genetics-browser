module Test.Main where

import Prelude
import Genetics.Browser.GlyphF.Canvas as Canvas
import Genetics.Browser.GlyphF.SVG as SVG
import Test.QuickCheck.Laws.Data as Data
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Genetics.Browser.Feature (Feature, ScreenFeature, feature, featureToScreen, withFeature)
import Genetics.Browser.Glyph (Glyph, circle, fill, rect, stroke)
import Genetics.Browser.GlyphPosition (GlyphPosition)
import Genetics.Browser.Units (Bp(..), MBp(..))
import Graphics.Canvas (getCanvasElementById, getContext2D, translate)
import Test.QuickCheck.Laws (QC)
import Type.Proxy (Proxy(..))
import Test.Units as Units


foreign import testGlyphPos :: Foreign -> String
foreign import showGlyphSVG :: Foreign -> Unit
foreign import addElementToDiv :: ∀ eff. String -> Element -> Eff ( dom :: DOM | eff ) Unit
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


exFeature1 :: Feature MBp ()
exFeature1 = feature { chr: "1", min: MBp (-5.0), max: MBp 5.0 }
exFeature2 :: Feature MBp ()
exFeature2 = feature { chr: "1", min: MBp 10.0, max: MBp 20.0 }
exFeature3 :: Feature Bp ()
exFeature3 = feature { chr: "1", min: Bp 60000.0, max: Bp 61000.0 }


glyph1 :: Glyph Unit
glyph1 = glyphify 0.0 (featureToScreen (MBp 0.0) (MBp 5.0) exFeature1)

glyph2 :: Glyph Unit
glyph2 = glyphify 50.0 (featureToScreen (MBp 0.0) (MBp 5.0) exFeature2)

glyph3 :: Glyph Unit
glyph3 = glyphify 100.0 (featureToScreen (Bp 0.0) (Bp 5.0) exFeature3)


glyphify :: Number -> ScreenFeature () -> Glyph Unit
glyphify y = withFeature $ \f -> do
  stroke "#ff0000"
  fill "#555555"
  rect { x: f.min, y: y } { x: f.max, y: y + 40.0 }



checkGlyphPosInstances :: ∀ e. QC e Unit
checkGlyphPosInstances = do
  Data.checkSemigroup prxGlyph
  Data.checkMonoid prxGlyph
  where
    prxGlyph = Proxy :: Proxy GlyphPosition

runBrowserTest :: QC _ Unit
runBrowserTest = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> log "couldn't find canvas"
    Just c  -> do
      ctx <- getContext2D c
      translate {translateX: -200.0, translateY: 0.0} ctx
      log "rendering glyph to canvas"
      traverse_ (Canvas.renderGlyph ctx) [glyph1, glyph2, glyph3]

  log "rendering glyph to svg"
  traverse_ (\g -> SVG.renderGlyph g >>= addElementToDiv "svgDiv") [glyph1, glyph2, glyph3]

  pure unit

main :: QC _ Unit
main = do
  checkGlyphPosInstances
  setOnLoad runBrowserTest
  Units.main
