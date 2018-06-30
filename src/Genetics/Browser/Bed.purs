module Genetics.Browser.Bed where

import Prelude

import Control.Coroutine (Producer, Transformer, transform)
import Control.Coroutine.Aff as Aff
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left))
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (wrap)
import Data.String as String
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, unV)
import Debug.Trace as Debug
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.AVar (new, put, take) as AVar
import Foreign (Foreign, MultipleErrors, readArray, renderForeignError)
import Genetics.Browser.Types (ChrId)
import Network.HTTP.Affjax as Affjax
import Network.HTTP.Affjax.Response as Affjax
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


validateBedChunk :: Array Foreign -> Validated (Array ParsedLine)
validateBedChunk d =
  case (traverse read d) :: Either MultipleErrors (Array RawBedLine) of

    Left err -> invalid $ map renderForeignError err
    Right ls -> traverse validLine ls

fetchBed :: String -> Aff (Array ParsedLine)
fetchBed url = do
  resp <- _.response <$> Affjax.get Affjax.json url

  let throwParseError = throwError <<< error <<< foldMap (_ <> ", ")

  case runExcept $ readArray $ unsafeCoerce resp of
    Left err -> throwParseError $ map renderForeignError err
    Right ls -> do
        unV throwParseError
            pure
            $ validateBedChunk ls
