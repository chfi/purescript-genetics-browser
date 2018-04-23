module Genetics.Browser.Track.Demo where

import Prelude

import Color (Color, black)
import Color.Scheme.Clrs (blue, navy, red)
import Color.Scheme.X11 (darkblue, darkgrey, lightgrey)
import Control.Coroutine (Producer, transform, ($~), (~~))
import Control.Monad.Aff (Aff, throwError)
import Control.Monad.Eff.Exception (error)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Filterable (class Filterable, filter, filterMap)
import Data.Foldable (class Foldable, any, fold, foldMap, or)
import Data.Lens (view, (^?))
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Pair (Pair(..))
import Data.Pair as Pair
import Data.Profunctor.Strong (fanout)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Variant (inj)
import Genetics.Browser.Track.Backend (DrawingN, DrawingV, Feature, LegendEntry, NPoint, OldRenderer, RenderedTrack, Label, _range, _single, groupToMap, mkIcon, negLog10, trackLegend, zipMapsWith)
import Genetics.Browser.Track.Bed (ParsedLine, chunkProducer, fetchBed, fetchForeignChunks, parsedLineTransformer)
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId))
import Genetics.Browser.Types.Coordinates (CoordSys, Normalized(Normalized), _Segments, aroundPair, pairSize, pairsOverlap)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, Point, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Math as Math
import Network.HTTP.Affjax as Affjax



type BedFeature = Feature { thickRange :: Pair Bp
                          , blocks :: Array (Pair Bp)
                          , geneId :: String
                          , geneName :: String
                          , chrId :: ChrId }


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
         -> Aff _ (Map ChrId (Array BedFeature))
getGenes cs url = do
  ls <- fetchBed url

  let fs = filterMap (bedToFeature cs) ls

  pure $ groupToMap _.feature.chrId $ fs

produceGenes :: CoordSys ChrId BigInt -> String
             -> Aff _
                (Producer
                 (Map ChrId (Array BedFeature))
                 (Aff _) Unit)
produceGenes cs url = do
  prod <- fetchForeignChunks url

  pure
    $ prod
    $~ parsedLineTransformer
    ~~ transform
       (groupToMap _.feature.chrId <<< filterMap (bedToFeature cs))


bedDraw :: BedFeature
        -> DrawingV
bedDraw gene = inj _range \w ->
  let (Pair l r) = gene.position
      toLocal x = w * (unwrap $ x / gene.frameSize)

      width = toLocal (r - l)

      exon block =
        let p@(Pair exL' size') = toLocal <$> block
            s = rectangle exL' zero size' 30.0
        in (outlined (outlineColor darkgrey <> lineWidth 1.0) s)
        <> (filled (fillColor lightgrey) s)

      introns _ =
        let s = rectangle 1.5 14.0 (width-1.5) 2.0
        in outlined (outlineColor black <> lineWidth 3.0) s
        <> filled   (fillColor black) s

      label _ =
        Drawing.text
          (font sansSerif 12 mempty)
             (2.5) (35.0)
             (fillColor black) gene.feature.geneName

      drawing =
        if width < one
        then mempty
        else \_ -> introns unit
                <> foldMap exon gene.feature.blocks
                <> label unit

  in { drawing, width }

geneRenderer :: OldRenderer BedFeature
geneRenderer = inj _single { draw: bedDraw, horPlace, verPlace: const (Normalized 0.10) }
  where horPlace {position, frameSize} =
          let f p = Normalized (unwrap $ p / frameSize)
          in inj _range $ map f position



fetchJsonChunks :: String
                -> Aff _ (Producer (Array Json) (Aff _) Unit)
fetchJsonChunks url = do
  json <- _.response <$> Affjax.get url

  case json ^? _Array of
    Nothing -> throwError $ error "Parse error: JSON is not an array"
    Just ls -> pure $ chunkProducer 512 ls


featureProd :: ∀ r1 r2.
               String
            -> (Json -> Maybe { feature :: { chrId :: ChrId | r1 } | r2 })
            -> Aff _
                 (Producer
                 (Map ChrId (Array { feature :: { chrId :: ChrId | r1 } | r2 }))
                 (Aff _) Unit)
featureProd url parse = do
  prod <- fetchJsonChunks url
  pure $ prod $~ (transform $ groupToMap _.feature.chrId <<< filterMap parse)


-- TODO bundle up all the producers into one function on a record of URLs
--      to a record of producers (maprecord? applyrecord?)
produceGWAS :: CoordSys ChrId _
            -> String
            -> Aff _
               (Producer
                (Map ChrId
                 (Array (GWASFeature ())))
                 (Aff _) Unit)
produceGWAS cs url = featureProd url $ gemmaJSONParse cs

produceAnnots :: CoordSys ChrId _
              -> String
              -> Aff _
                 (Producer
                  (Map ChrId
                   (Array (Annot ())))
                   (Aff _) Unit)
produceAnnots cs url = featureProd url $ (annotJSONParse cs)


fetchJSON :: ∀ a.
             (Json -> Maybe a)
          -> String
          -> Aff _ (Array a)
fetchJSON p url = do
  json <- _.response <$> Affjax.get url

  case json ^? _Array of
    Nothing -> throwError $ error "Parse error: JSON is not an array"
    Just as -> case traverse p as of
      Nothing -> throwError $ error "Failed to parse JSON features"
      Just fs -> pure $ fs


type GWASFeature r = Feature { score :: Number
                             , chrId :: ChrId
                             , name  :: String | r }

gemmaJSONParse :: CoordSys ChrId BigInt
               -> Json
               -> Maybe (GWASFeature ())
gemmaJSONParse cs j = do
  obj <- j ^? _Object
  chrId <- ChrId <$> obj ^? ix "chr" <<< _String
  pos   <- Bp    <$> obj ^? ix "ps"  <<< _Number
  score <-           obj ^? ix "p_wald"  <<< _Number
  name  <-           obj ^? ix "rs"  <<< _String

  chrSize <- (Bp <<< BigInt.toNumber <<< pairSize) <$> Map.lookup chrId (view _Segments cs)

  pure { position: Pair pos pos
       , frameSize: chrSize
       , feature: { score, chrId, name }
       }


type AnnotRow r = ( geneID :: String
                   , desc :: String
                   , name :: String
                   , chrId :: ChrId | r )
type Annot r = Feature {| AnnotRow r}


annotJSONParse :: CoordSys ChrId BigInt
              -> Json
              -> Maybe (Annot ())
annotJSONParse cs j = do
  obj    <- j ^? _Object
  geneID <- obj ^? ix "Gene stable ID" <<< _String
  desc   <- obj ^? ix "Gene description" <<< _String
  name   <- obj ^? ix "Gene name" <<< _String
  start  <- Bp    <$> obj ^? ix "Gene start (bp)"  <<< _Number
  end    <- Bp    <$> obj ^? ix "Gene end (bp)"  <<< _Number
  chrId  <- ChrId <$> obj ^? ix "ChrId" <<< _String

  chrSize <- (Bp <<< BigInt.toNumber <<< pairSize) <$> Map.lookup chrId (view _Segments cs)

  pure { position: Pair start end
       , frameSize: chrSize
       , feature: { geneID, desc, name, chrId }
       }


fetchAnnotJSON :: CoordSys ChrId BigInt -> String
               -> Aff _ (Array (Annot ()))
fetchAnnotJSON cs str = fetchJSON (annotJSONParse cs) str


getGWAS :: CoordSys ChrId BigInt -> String
        -> Aff _ (Map ChrId (Array (GWASFeature ())))
getGWAS cs url = groupToMap _.feature.chrId
                  <$> filter (\f -> Pair.fst (f.position) >= wrap zero)
                  <$> fetchJSON (gemmaJSONParse cs) url


getAnnotations :: CoordSys ChrId BigInt -> String
               -> Aff _ (Map ChrId (Array (Annot ())))
getAnnotations cs url = groupToMap _.feature.chrId
                        <$> fetchAnnotJSON cs url


getAnnotations' :: CoordSys ChrId BigInt
                -> String
                -> Aff _ (Array (Annot ()))
getAnnotations' = fetchAnnotJSON




inRangeOf :: ∀ rA rB.
             Bp
          -> { position :: Pair Bp | rA }
          -> { position :: Pair Bp | rB }
          -> Boolean
inRangeOf r a b = pairsOverlap a.position
                             $ r `aroundPair` b.position


interestingAnnots :: ∀ rA rS f.
                     Filterable f
                  => Bp
                  -> Map ChrId (Array (GWASFeature rS))
                  -> f (Annot rA)
                  -> f (Annot rA)
interestingAnnots radius snps = filter (any (any (inRangeOf radius)) snps)




-- TODO Configgable Annotation -> LegendEntry function (somehow?!)
annotLegendEntry :: ∀ r. Annot r -> LegendEntry
annotLegendEntry a =
  if (String.length a.feature.name) `mod` 2 == 0
    then mkIcon blue "even name"
    else mkIcon red  "odd name"


annotLegendTest :: ∀ f r.
                   Foldable f
                => Functor f
                => Map ChrId (f (Annot r))
                -> Array LegendEntry
annotLegendTest fs = trackLegend annotLegendEntry as
  where as = Array.concat
             $ Array.fromFoldable
             $ Array.fromFoldable <$> Map.values fs





------------ new renderers~~~~~~~~~


placeScored :: ∀ r1 r2.
            { min :: Number, max :: Number | r1 }
         -> Feature { score :: Number | r2 }
         -> NPoint
placeScored {min, max} { frameSize, position: (Pair l _), feature } = {x, y}
  where x = Normalized $ unwrap $ l / frameSize
        y = Normalized $ ((negLog10 feature.score) - min) / (max - min)


dist :: Point -> Point -> Number
dist p1 p2 = Math.sqrt $ x' `Math.pow` 2.0 + y' `Math.pow` 2.0
  where x' = p1.x - p2.x
        y' = p1.y - p2.y



filterSig :: ∀ r1 r2.
             { sig :: Number | r1 }
          -> Map ChrId (Array (GWASFeature r2))
          -> Map ChrId (Array (GWASFeature r2))
filterSig {sig} = map (filter isSignificant)
  where sig' = 10.0 `Math.pow` (-sig)
        isSignificant {feature} = feature.score <= sig'




renderGWAS :: ∀ r.
              { min :: Number, max :: Number | r }
           -> Canvas.Dimensions
           -> Map ChrId (Array (GWASFeature ()))
           -> Map ChrId (Pair Number)
           -> RenderedTrack (GWASFeature ())
renderGWAS verscale cdim snps =
  let features :: Array (GWASFeature ())
      features = fold snps

      radius = 3.75

      drawing =
          let color = darkblue
              c = circle 0.0 0.0 radius
              out = outlined (outlineColor color) c
              fill = filled (fillColor color) c
          in out <> fill

      drawings :: Array (Tuple (GWASFeature ()) Point) -> Array DrawingN
      drawings pts = let (Tuple _ points) = Array.unzip pts
                     in [{ drawing, points }]

      npointed :: Map ChrId (Array (Tuple (GWASFeature ()) NPoint))
      npointed = (map <<< map) (fanout id (placeScored verscale)) snps

      rescale :: Pair Number -> NPoint -> Point
      rescale seg npoint =
        let (Pair offset _) = seg
            x = offset + (pairSize seg) * (unwrap npoint.x)
            y = cdim.height * (one - unwrap npoint.y)
        in {x, y}

      pointed :: Map ChrId (Pair Number)
              -> Array (Tuple (GWASFeature ()) Point)
      pointed segs = fold $ zipMapsWith (\s p -> (map <<< map) (rescale s) p) segs npointed

      overlaps :: Array (Tuple (GWASFeature ()) Point)
               -> Number -> Point
               -> Array (GWASFeature ())
      overlaps pts radius' pt = filterMap covers pts
        where covers :: Tuple (GWASFeature ()) Point -> Maybe (GWASFeature ())
              covers (Tuple f fPt)
                | dist fPt pt <= radius + radius'   = Just f
                | otherwise = Nothing

  in \seg -> let pts = pointed seg
             in { features
                , drawings: drawings pts
                , labels: mempty
                , overlaps: overlaps pts }



renderAnnot :: ∀ r.
               { min :: Number, max :: Number | r }
            -> Canvas.Dimensions
            -> Map ChrId (Array (Annot (score :: Number)))
            -> Map ChrId (Pair Number)
            -> RenderedTrack (Annot (score :: Number))
renderAnnot verscale cdim annots =
  let features :: Array (Annot (score :: Number))
      features = fold annots

      tailHeight = 0.15

      drawing :: Tuple (Annot (score :: Number)) Point -> DrawingN
      drawing (Tuple an pt) = { drawing: tail <> lg.icon , points: [pt] }
        where lg = annotLegendEntry an
              tail = Drawing.outlined (lineWidth 1.3 <> outlineColor black)
                     $ Drawing.path
                       [ {x: 0.0, y: 0.0 }
                       , {x: 0.0, y: tailHeight * cdim.height }]


      drawings :: Array (Tuple (Annot (score :: Number)) Point) -> Array DrawingN
      drawings = map drawing

      label :: Tuple (Annot (score :: Number)) Point -> Label
      label (Tuple an {x,y}) =
        { text: an.feature.name
        , point: { x, y: y - 12.0 }  }

      labels :: Array (Tuple (Annot (score :: Number)) Point) -> Array Label
      labels = map label

      npointed :: Map ChrId (Array (Tuple (Annot (score :: Number)) NPoint))
      npointed = (map <<< map) (fanout id place) annots
        where place p = let p' = placeScored verscale p
                        in { x: p'.x
                           , y: Normalized $ min 1.0 (unwrap p'.y + tailHeight) }

      rescale :: Pair Number -> NPoint -> Point
      rescale seg npoint =
        let (Pair offset _) = seg
            x = offset + (pairSize seg) * (unwrap npoint.x)
            y = cdim.height * (one - unwrap npoint.y)
        in {x, y}

      pointed :: Map ChrId (Pair Number)
              -> Array (Tuple (Annot (score :: Number)) Point)
      pointed segs = fold $ zipMapsWith (\s p -> (map <<< map) (rescale s) p) segs npointed

      overlaps :: Number -> Point -> Array (Annot (score :: Number))
      overlaps r p = mempty

  in \seg -> let pts = pointed seg
             in { features
                , drawings: drawings pts
                , labels: labels pts
                , overlaps: overlaps  }
