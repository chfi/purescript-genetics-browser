module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Genetics.Browser.Feature (Feature(..), scaleFeature, translateFeature)
import Genetics.Browser.Glyph (Glyph, circle, fill, rect, stroke)
import Genetics.Browser.GlyphF.Canvas as Canvas
import Genetics.Browser.GlyphF.SVG as SVG
import Genetics.Browser.GlyphPosition (GlyphPosition)
import Graphics.Canvas (getCanvasElementById, getContext2D, translate)
import Test.QuickCheck.Laws (QC)
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))


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


exFeature1 :: Feature ()
exFeature1 = Feature { min: -5.0, max: 5.0 }
exFeature2 :: Feature ()
exFeature2 = Feature { min: 10.0, max: 20.0 }
exFeature3 :: Feature ()
exFeature3 = Feature { min: 600.0, max: 610.0 }


glyph1 :: Glyph Unit
glyph1 = glyphify 0.0 (translateFeature 0.0 (scaleFeature 5.0 exFeature1))

glyph2 :: Glyph Unit
glyph2 = glyphify 50.0 (translateFeature 0.0 (scaleFeature 5.0 exFeature2))

glyph3 :: Glyph Unit
glyph3 = glyphify 100.0 (translateFeature 0.0 (scaleFeature 5.0 exFeature3))


glyphify :: Number -> Feature () -> Glyph Unit
glyphify y (Feature f) = do
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
