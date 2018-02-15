module Genetics.Browser.Track.Bed where

import Prelude

import Color (Color, black)
import Color.Scheme.Clrs (aqua, blue, fuchsia, green, lime, maroon, navy, olive, orange, purple, red, teal, yellow)
import Control.Monad.Aff (Aff, throwError)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept)
import Data.Argonaut (Json, JObject, _Array, _Number, _Object, _String)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..), note)
import Data.Exists (Exists, mkExists)
import Data.Filterable (filtered)
import Data.Foldable (class Foldable)
import Data.Foreign (Foreign, MultipleErrors, renderForeignError)
import Data.Int as Int
import Data.Lens (re, to, view, (^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Pair (Pair(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (inj)
import Genetics.Browser.Track.Backend (ChrCtx, DrawingV, Feature, LegendEntry, Renderer, Track(Track), VScale, _point, _range, featureInterval, groupToChrs, horPlace, mkIcon, trackLegend, verPlace)
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId), _ChrId)
import Genetics.Browser.Types.Coordinates (CoordSys, Normalized(Normalized), _Segments, pairSize)
import Graphics.Drawing (circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Network.HTTP.Affjax as Affjax
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


type BedChrom i c r = ( chromId :: i, chromStart :: c, chromEnd :: c | r )

type BedLineName r = ( lineName :: String | r )
-- a valid score is >= 0 and <= 1000
type BedScore r = ( score :: Int | r )


  -- bad type!!! strand is only one of three characters after all
type BedStrand r = ( strand :: String | r )

type BedThick c r = ( thickStart :: c, thickEnd :: c | r )

-- this is pretty bad too~~
type BedRGB r = ( itemRgb :: Array Int | r )


type BedBlocks c r = ( blockCount :: Int
                     , blockSizes :: Array c
                     , blockStarts :: Array c | r )

type BedGene r = ( geneId :: String
                 , geneName :: String | r )

type BedTranscript r = ( "type" :: String
                       , tags :: Array String | r )


type BedLine i c =
  Record
  (BedChrom i c
   (BedLineName
    (BedScore
     (BedStrand
      (BedThick c
       (BedRGB
        (BedBlocks c
         (BedGene
          (BedTranscript ())))))))))


type RawBedLine = BedLine String Int

type Validated = V (NonEmptyList String)


-- TODO this should compare to browser coord sys used
validChrId :: forall r.
              { chromId :: String | r }
           -> Validated { chromId :: ChrId  }
validChrId r = pure { chromId: wrap r.chromId }

validChrRange :: forall r.
                 { chromStart :: Int, chromEnd :: Int | r }
              -> Validated { chromStart :: BigInt, chromEnd :: BigInt }
validChrRange r
-- this is probably too strict.
  | r.chromStart > r.chromEnd = invalid $ pure "feature start > feature end"
  | otherwise = pure $ { chromStart: BigInt.fromInt r.chromStart
                       , chromEnd: BigInt.fromInt r.chromEnd }


validScore :: forall r.
              Record (BedScore r)
           -> Validated (Record (BedScore ()))
validScore { score } = pure { score }

validStrand :: forall r.
               Record (BedStrand r)
            -> Validated (Record (BedStrand ()))
validStrand { strand } = pure { strand }


-- this could be deduped w/ lens and/or sproxy
validThickRange :: forall r.
                 Record (BedThick Int r)
              -> Validated (Record (BedThick BigInt ()))
validThickRange r
  | r.thickStart > r.thickEnd = invalid $ pure "feature thick start > end"
  | otherwise = pure $ { thickStart: BigInt.fromInt r.thickStart
                       , thickEnd: BigInt.fromInt r.thickEnd }

validRGB :: forall r.
            { itemRgb :: Array Int | r }
         -> Validated { itemRgb :: Array Int  }
validRGB {itemRgb} = pure {itemRgb}

validBlocks :: forall r.
            Record (BedBlocks Int r)
         -> Validated (Record (BedBlocks BigInt ()))
validBlocks r = pure { blockCount: r.blockCount
                     , blockSizes: BigInt.fromInt  <$> r.blockSizes
                     , blockStarts: BigInt.fromInt <$> r.blockStarts }

validGene :: forall r.
             Record (BedGene r)
          -> Validated (Record (BedGene ()))
validGene {geneId, geneName} = pure {geneId, geneName}

validTranscript :: forall r.
                   Record (BedTranscript r)
                -> Validated (Record (BedTranscript ()))
validTranscript r = pure { "type": r."type", tags: r.tags }

validateBed :: Foreign -> Validated (Array (BedLine ChrId BigInt))
validateBed d =
  case (runExcept $ read d) :: Either MultipleErrors (Array RawBedLine) of
    Left err -> invalid $ map renderForeignError err
    Right ls ->
      let f :: _
          f l = { chromId:_, chromRange:_, name: _
                , score: _, thick: _, rgb: _
                , blocks: _, gene: _, transcript: _ }
              <$> validChrId l
              <*> validChrRange l
              <*> validScore l
              <*> validStrand l
              <*> validRGB l
              <*> validBlocks l
              <*> validGene l
              <*> validTranscript l
       in unsafeCoerce unit
