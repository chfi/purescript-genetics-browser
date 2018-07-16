module Genetics.Browser.Bed where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left))
import Data.Filterable (partitionMap)
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Pair (Pair(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Data.Validation.Semigroup (V, invalid, unV)
import Effect.Aff (Aff, delay, error, throwError)
import Foreign (Foreign, ForeignError(..), MultipleErrors, readArray, renderForeignError)
import Genetics.Browser (Feature, groupToMap)
import Genetics.Browser.Coordinates (CoordSys, _Segments, pairSize)
import Genetics.Browser.Types (Bp, ChrId)
import Network.HTTP.Affjax (get) as Affjax
import Network.HTTP.Affjax.Response (json) as Affjax
import Prim.RowList (kind RowList)
import Simple.JSON (read)
import Unsafe.Coerce (unsafeCoerce)


{- NB this deals with JSON that's currently specified to work with a
bespoke conversion of the gencode.bb genes file from biodalliance.org.
in other words, it is garbage

example converted line:

{"chrom":"chr1"
,"chromStart":"5083172"
,"chromEnd":"5162549"
,"name":"ENSMUST00000044369.7"
,"score":"1000"
,"strand":"+"
,"thickStart":"5084450"
,"thickEnd":"5162162"
,"itemRgb":"0,0,0"
,"blockCount":"14"
,"blockSizes":"106,147,103,90,114,105,54,98,193,179,126,102,114,445"
,"blockStarts":"0,1244,5836,10190,12442,14856,17897,34217,41104,49911,52639,60577,66775,78932"
,"geneId":"ENSMUSG00000033793.7"
,"geneName":"Atp6v1h"
,"type":"protein_coding"
,"tags":"basic,appris_principal,CCDS"}

-}


type BedLine count method attrs ident coord many =
  ( chrom :: ident
  , chromStart :: coord
  , chromEnd :: coord
  , name :: String
  , score :: count
  , strand :: String
  , thickStart :: coord
  , thickEnd :: coord
  , itemRgb :: String
  , blockCount :: count
  , blockSizes :: many
  , blockStarts :: many
  , geneId :: String
  , geneName :: String
  , "type" :: method
  , tags :: attrs
  )


type RawBedLine = Record (BedLine String String String String String String)

type ParsedLine = Record (BedLine Int String String ChrId BigInt (Array BigInt))

type Validated = V (NonEmptyList String)


-- TODO this should compare to browser coord sys used
validChrId :: String -> Validated ChrId
              -- TODO super duper cool hardcoded string transformation to go from "chr1" to "1" which everything else uses atm
validChrId = pure <<< wrap <<< String.drop 3

validInt :: String -> Validated Int
validInt s = case Int.fromString s of
  Nothing -> invalid $ pure $ "Error parsing int " <> s
  Just i  -> pure i

validBigInt :: String -> Validated BigInt
validBigInt s = case BigInt.fromString s of
  Nothing -> invalid $ pure $ "Error parsing int " <> s
  Just i  -> pure i

validList :: String -> V (NonEmptyList String) (Array BigInt)
validList x = traverse validBigInt (String.split (wrap ",") x)



-- TODO validate `*Start` and `*End` by providing a coord sys and comparing thick* to chrom* etc.
-- -- TODO more validation & parsing (e.g. of `itemRgb` and `strand` could be done, but not necessary ATM)
validLine :: RawBedLine -> Validated ParsedLine
validLine l = (l { chrom = _
                 , chromStart = _, chromEnd = _
                 , score = _
                 , thickStart = _, thickEnd = _
                 , blockCount = _
                 , blockSizes = _, blockStarts = _})
           <$> validChrId l.chrom
           <*> validBigInt l.chromStart <*> validBigInt l.chromEnd
           <*> validInt l.score
           <*> validBigInt l.thickStart <*> validBigInt l.thickEnd
           <*> validInt l.blockCount
           <*> validList l.blockSizes
           <*> validList l.blockStarts



type BedFeature = Feature { thickRange :: Pair Bp
                          , blocks :: Array (Pair Bp)
                          , geneId :: String
                          , geneName :: String
                          , chrId :: ChrId }


parseBed :: CoordSys ChrId BigInt
         -> Foreign
         -> Either MultipleErrors BedFeature
parseBed cs o = do
  raw <- read o
  parsed <- unV (Left <<< map ForeignError) pure $ validLine raw

  maybe
    (Left $ pure $ ForeignError "Error parsing BedFeature")
    pure
    $ bedToFeature cs parsed



bedToFeature :: CoordSys ChrId BigInt -> ParsedLine -> Maybe BedFeature
bedToFeature cs pl = do
  seg@(Pair offset _ ) <- cs ^? _Segments <<< ix pl.chrom

  -- TODO validate ranges maybe, idk
  let toBp :: BigInt -> Bp
      toBp = wrap <<< BigInt.toNumber

      frameSize = toBp $ pairSize seg
      position = toBp  <$> Pair pl.chromStart pl.chromEnd
      thickRange = toBp  <$> Pair pl.thickStart pl.thickEnd

      blocks = Array.zipWith
                 (\start size -> map toBp (Pair start size))
                 pl.blockStarts pl.blockSizes

  pure { position
       , frameSize
       , feature: { thickRange
                  , blocks
                  , geneId: pl.geneId
                  , geneName: pl.geneName
                  , chrId: pl.chrom
                  }
       }


getGenes :: CoordSys ChrId BigInt
         -> String
         -> Aff (Map ChrId (Array BedFeature))
getGenes cs url = do
  resp <- _.response <$> Affjax.get Affjax.json url

  let throwParseError = throwError <<< error <<< foldMap (_ <> ", ")

  array <- case runExcept $ readArray $ unsafeCoerce resp of
    Left err -> throwParseError $ map renderForeignError err
    Right ls -> pure ls

  let chunkSize = 100

      chunkF l = if List.null l
                 then Nothing
                 else Just (Tuple (List.take chunkSize l) (List.drop chunkSize l))

      chunks = unfoldr chunkF
               $ List.fromFoldable array

  let process :: List Foreign -> Aff _
      process chnk = do
        -- if the delay isn't done before the partitionMap, the
        -- traverse lags for several seconds at the start, because ___
        delay $ wrap 0.1
        let o = partitionMap (parseBed cs) chnk
        pure o.right

  processedChunks <- traverse process chunks

  let output = map Array.fromFoldable
               $ groupToMap _.feature.chrId
               $ List.concat processedChunks

  pure output
