module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
-- import Jack (jackMain)
import Test.Coordinates as Coordinates
import Test.Cached as Cached
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)


main :: Eff _ Unit
main = do
  -- jackMain [  "Test.Units"
  --          ]

  run [consoleReporter] do
    Cached.spec
    Coordinates.spec
