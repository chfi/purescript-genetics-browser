module Genetics.Browser.Biodalliance.Source.IPFS where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, throw)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Genetics.Browser.Biodalliance.Source (FetchFunction, ForeignSourceBase, Source, createSource)
import IPFS (IPFS, IPFSEff)
import IPFS.Files as Files
import IPFS.Types (IPFSEff, IPFSPath(..))
import Node.Encoding (Encoding(..))
import Node.Stream as Stream


ipfsFetch :: ∀ a eff.
             IPFS
          -> String
          -> (String -> Maybe a)
          -> FetchFunction (Aff ( ipfs :: IPFSEff
                                , exception :: EXCEPTION | eff ) ) a
ipfsFetch ipfs path parse = \chr min max -> do
  stream <- Files.cat ipfs (IPFSPathString path)

  liftEff $ Stream.pause stream

  file <- liftEff $ Stream.readString stream Nothing UTF8

  case file >>= parse of
    Just a  -> pure a
    Nothing -> throwError $ error "Failed"


ipfsSource :: ∀ a.
              IPFS
           -> String
           -> (String -> Maybe a)
           -> ForeignSourceBase
           -> Source a
ipfsSource ipfs path parser fsb =
  createSource fsb $ ipfsFetch ipfs path parser
