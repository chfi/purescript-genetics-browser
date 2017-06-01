module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow)
import DOM (DOM)
import DOM.Node.Types as DOM
import Data.Argonaut (jsonParser, toArray)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Genetics.Browser.Cytoscape as Cy
import Genetics.Browser.Cytoscape.Collection
import Genetics.Browser.Cytoscape.Types
import Genetics.Browser.Feature (Feature(..), ScreenFeature, featureToScreen)
import Genetics.Browser.Glyph (Glyph, circle, fill, rect, stroke)
import Genetics.Browser.GlyphF.Canvas as Canvas
import Genetics.Browser.GlyphF.SVG as SVG
import Genetics.Browser.GlyphPosition (GlyphPosition)
import Genetics.Browser.Units (Bp(..), MBp(..))
import Graphics.Canvas (getCanvasElementById, getContext2D, translate)
import Prelude
import Test.QuickCheck.Laws (QC)
import Test.QuickCheck.Laws.Data as Data
import Test.Units as Units
import Type.Proxy (Proxy(..))



foreign import testGlyphPos :: Foreign -> String
foreign import showGlyphSVG :: Foreign -> Unit
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

checkGlyphPosInstances :: ∀ e. QC e Unit
checkGlyphPosInstances = do
  Data.checkSemigroup prxGlyph
  Data.checkMonoid prxGlyph
  where
    prxGlyph = Proxy :: Proxy GlyphPosition


testCytoscape :: Eff _ Unit
testCytoscape = do
  case jsonParser "[{\"data\": { \"id\": \"a\" }},{\"data\": { \"id\": \"b\" }},{\"data\": { \"id\": \"ab\", \"source\": \"a\", \"target\": \"b\" }}]" of
    Left e     -> log $ "Could not parse Cytoscape elements JSON: " <> e
    Right json -> case toArray json of
      Nothing    -> log $ "Could not parse Cytoscape elements into Array."
      Just ar    -> do
        cy <- Cy.cytoscape Nothing (Just ar)
        eles <- Cy.graphGetCollection cy
        let edges = filter isEdge eles
            nodes = filter isNode eles
        log $ "All elements"
        logShow $ collectionJson eles
        log $ "Filtered edges"
        logShow $ collectionJson edges
        log $ "Filtered nodes"
        logShow $ collectionJson nodes
        log $ "Union of filtered"
        logShow $ collectionJson $ edges `union` nodes
        log $ "Are the collections equal?"
        logShow $ eles == nodes `union` edges
        log $ "Edges are contained in main collection"
        logShow $ eles `contains` edges
        log $ "Nodes are contained in main collection"
        logShow $ eles `contains` nodes
        log $ "Edges are not contained in nodes"
        logShow $ not $ nodes `contains` eles


runBrowserTest :: QC _ Unit
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

main :: QC _ Unit
main = do
  checkGlyphPosInstances
  testCytoscape
  setOnLoad runBrowserTest
  Units.main
