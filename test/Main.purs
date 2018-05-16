module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
-- import Jack (jackMain)
import Test.Coordinates as Coordinates
-- import Test.Cytoscape as Cytoscape
-- import Test.Events as Events
-- import Test.Glyph as Glyph
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
-- import Test.Track as Track


main :: Eff _ Unit
main = do
  -- jackMain [ "Test.Glyph"
  --          , "Test.Units"
  --          ]

  run [consoleReporter] do
    -- Cytoscape.spec
    -- Config.spec
    -- Track.spec
    -- Events.spec
    Coordinates.spec


  -- Glyph.svgCanvasTest
