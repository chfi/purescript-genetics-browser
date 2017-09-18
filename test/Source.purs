module Test.Source where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Genetics.Browser.Biodalliance.Source (Source, createSource)
import Genetics.Browser.Units (Bp(..), Chr(..))
import Jack (jackMain)
import Test.Config as Config
import Test.Cytoscape as Cytoscape
import Test.Events as Events
import Test.Glyph as Glyph
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Test.Track as Track

fetchFun :: Chr -> Bp -> Bp -> Aff _ String
fetchFun _ _ _ = pure "hello world"

foreign import testFetch :: ∀ eff a. a -> Source a -> Eff eff Unit

testSource :: Eff _ Unit
testSource = do
  let src = createSource fetchFun
  log "Test Fetch equal:"
  testFetch "hello world" src
