module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Config as Config
import Test.Glyph as Glyph
import Test.Cytoscape as Cytoscape
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Test.Track as Track
-- import Test.Units as Units
import Test.Events as Events


main :: Eff _ Unit
main = do
  run [consoleReporter] do
    Glyph.spec

    -- Units.spec
    Cytoscape.spec
    Config.spec
    Track.spec
    Events.spec

  Glyph.svgCanvasTest
