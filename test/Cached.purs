module Test.Cached where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Int (even)
import Data.String as String
import Genetics.Browser.Cached (Cached, applyCache, cache, input, output)
import Global.Unsafe (unsafeStringify)
import Test.Spec (Spec, describe, it)

spec :: Spec _ Unit
spec = do
  describe "Cached" do

    it "Can get the current input output" do
      liftEff do
        f' <- cache even 0
        fI <- input  f'
        fO <- output f'
        log "f:"
        log $ " \\- input:  " <> unsafeStringify fI
        log $ " |- output: " <> unsafeStringify fO


      liftEff do
        g' <- cache String.length "hello"
        log "\n\ng:"
        gI <- input  g'
        gO <- output g'
        log $ " \\- input:  " <> unsafeStringify gI
        log $ " |- output: " <> unsafeStringify gO

      pure unit
