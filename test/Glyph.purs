module Test.Glyph where
       -- ( svgCanvasTest
       -- , prop_semigroup
       -- , prop_monoid
       -- ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import DOM (DOM)
import DOM.Node.Types as DOM
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Data.Traversable (traverse_)
import Genetics.Browser.Feature (Feature(..), ScreenFeature, featureToScreen)
import Genetics.Browser.Glyph (Glyph, circle, fill, rect, stroke)
import Genetics.Browser.GlyphF.Canvas as Canvas
import Genetics.Browser.GlyphF.SVG as SVG
import Genetics.Browser.GlyphPosition (GlyphPosition(..))
import Genetics.Browser.Types (Bp(..), ChrId(..), MBp(..))
import Graphics.Canvas (getCanvasElementById, getContext2D, translate)
-- import Jack (Gen, Property, chooseInt, forAll, forAllRender, property)


type ThreeGlyphs = {l :: GlyphPosition, c :: GlyphPosition, r :: GlyphPosition}

renderGlyphs :: ThreeGlyphs -> String
renderGlyphs {l,c,r} = "{ l: " <> show l <> ", c:" <> show c <> ", r:" <> show r <> "}"

-- genGlyphPosition :: Gen GlyphPosition
-- genGlyphPosition = do
--   let cf = toNumber <$> chooseInt (-10000000) (10000000)
--   min <- cf
--   max <- cf
--   minY <- cf
--   maxY <- cf
--   pure $ GlyphPos { min, max, minY, maxY }

-- genThreeGlyphs :: Gen ThreeGlyphs
-- genThreeGlyphs = do
--   l <- genGlyphPosition
--   c <- genGlyphPosition
--   r <- genGlyphPosition
--   pure $ {l, c, r}

-- prop_semigroup :: Property
-- prop_semigroup =
--   forAllRender renderGlyphs genThreeGlyphs \pos ->
--     property $ (pos.l <> pos.c) <> pos.r == pos.l <> (pos.c <> pos.r)

-- prop_monoid :: Property
-- prop_monoid =
--   forAll genGlyphPosition \pos ->
--     property $ pos <> mempty == pos

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
exFeature1 = Feature (ChrId "1") (MBp (-5.0)) (MBp 5.0) unit
exFeature2 :: Feature MBp Unit
exFeature2 = Feature (ChrId "1") (MBp 10.0) (MBp 20.0) unit
exFeature3 :: Feature Bp Unit
exFeature3 = Feature (ChrId "1") (Bp 60000.0) (Bp 61000.0) unit


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
