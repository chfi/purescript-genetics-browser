module Genetics.Browser.Biodalliance.Source.IPFS where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, Json, decodeJson, jsonParser)
import Data.Either (Either(..))
import Data.Traversable (class Traversable, traverse)
import Genetics.Browser.Types (Bp, Chr)
import IPFS (IPFS, IPFSEff)
import IPFS.Files as Files
import IPFS.Types (IPFSPath(..))
import Node.Encoding (Encoding(..))
import Node.Stream (Readable)
import Node.Stream as Stream


affOnDataString :: Readable _ _
                -> Encoding
                -> Aff _ String
affOnDataString stream encoding =
  makeAff (\error success -> Stream.onDataString stream encoding success)


fetchIPFSFeatures :: ∀ f a.
                     Functor f
                  => DecodeJson (f Json)
                  => Traversable f
                  => IPFS
                  -> (Json -> Either String a)
                  -> String
                  -> Chr -> Bp -> Bp -> Aff _ (f a)
fetchIPFSFeatures ipfs parse path chr min max = do
  str  <- Files.cat ipfs (IPFSPathString path)
  raw  <- jsonParser <$> affOnDataString str UTF8
  case raw >>= decodeJson >>= traverse parse of
    Left err -> throwError $ error err
    Right fs -> pure fs
