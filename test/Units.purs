module Test.Units where

import Prelude
import Test.QuickCheck.Laws.Data as Data
import Control.Monad.Eff.Console (log)
import Genetics.Browser.Units (class HCoordinate, Bp(..), MBp(..), bp, mbp)
import Test.QuickCheck (quickCheck')


checkUnitIso :: _
checkUnitIso = do
  log "Checking isomorphism for coordinate units"
  log "Bp:"
  quickCheck' 1000 (isomorphism :: Bp -> Boolean)
  log "MBp:"
  quickCheck' 1000 (isomorphism :: MBp -> Boolean)

  where isomorphism :: ∀ c. (HCoordinate c) => c -> Boolean
        isomorphism x = bp x  - bp (mbp x) < Bp 0.000001 &&
                        mbp x - mbp (bp x) < MBp 0.000001

main :: _
main = do
  checkUnitIso
