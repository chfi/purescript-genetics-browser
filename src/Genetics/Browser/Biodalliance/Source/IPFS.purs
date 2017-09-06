module Genetics.Browser.Biodalliance.Source.IPFS where

import Prelude

import Genetics.Browser.Biodalliance.Source (createSource)
import IPFS (IPFS, IPFSEff)
import IPFS.Files as Files
import Node.Encoding (Encoding(..))


ipfsFetch :: ∀ a.
             IPFS
          -> String
          -> (String -> Maybe a)
          -> FetchFunction (Aff _) (Maybe a)
          -- -> Chr -> Bp -> Bp -> (Aff _ a)
ipfsFetch ipfs path parser = \chr min max -> do
  stream <- Files.cat ipfs path
  parser <$> liftEff $ Stream.readString stream


ipfsSource :: ∀ a.
              IPFS
           -> String
           -> (String -> Maybe a)
           -> ForeignSourceBase
           -> Source (Maybe a)
ipfsSource ipfs path parser fsb =
  createSource fsb $ ipfsFetch ipfs path parser
