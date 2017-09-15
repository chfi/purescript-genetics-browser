module Genetics.Browser.Biodalliance.Source.IPFS where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throw)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, decodeJson, jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Genetics.Browser.Biodalliance.Source (FetchFunction, Source, createSource)
import Genetics.Browser.Units (Bp(..), Chr(..))
import IPFS (IPFS, IPFSEff)
import IPFS.Files as Files
import IPFS.Types (IPFSEff, IPFSPath(..))
import Node.Encoding (Encoding, Encoding(..))
import Node.Stream (Readable)
import Node.Stream as Stream


affOnDataString :: Readable _ _
                -> Encoding
                -> Aff _ String
affOnDataString stream encoding =
  makeAff (\error success -> Stream.onDataString stream encoding success)


fetchIPFSFeature :: ∀ a.
                    IPFS
                 -> (Json -> Either String a)
                 -> String
                 -> Chr -> Bp -> Bp -> Aff _ (Array a)
fetchIPFSFeature ipfs parse path chr min max = do
  str  <- Files.cat ipfs (IPFSPathString path)
  raw  <- jsonParser <$> affOnDataString str UTF8
  case raw >>= decodeJson >>= traverse parse of
    Left err -> throwError $ error err
    Right fs -> pure fs
