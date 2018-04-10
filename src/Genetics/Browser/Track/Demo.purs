module Genetics.Browser.Track.Demo where

import Prelude

import Color (Color, black)
import Color.Scheme.Clrs (blue, navy, red)
import Color.Scheme.X11 (darkgrey, lightgrey)
import Control.Coroutine (Producer, transform, ($~), (~~))
import Control.Monad.Aff (Aff, throwError)
import Control.Monad.Eff.Exception (error)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Exists (Exists, mkExists)
import Data.Filterable (filterMap, filtered)
import Data.Foldable (class Foldable, fold, foldMap)
import Data.Lens (view, (^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Pair (Pair(..))
import Data.Profunctor.Strong (fanout)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (case_, inj, onMatch)
import Genetics.Browser.Track.Backend (DrawingN, DrawingV, Feature, LegendEntry, NPoint, Rendered, Renderer, SingleGlyph, Track(Track), VScale, _batch, _point, _range, _single, groupToChrs, horPlace, mkIcon, trackLegend, zipMapsWith)
import Genetics.Browser.Track.Bed (ParsedLine, chunkProducer, fetchBed, fetchForeignChunks, parsedLineTransformer)
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId))
import Genetics.Browser.Types.Coordinates (CoordSys, CoordSysView, Normalized(Normalized), ViewScale, _Segments, pairSize, viewScale)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, Point, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Math as Math
import Network.HTTP.Affjax as Affjax
import Unsafe.Coerce (unsafeCoerce)


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

  pure $ groupToChrs _.feature.chrId $ fs

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
       (groupToChrs _.feature.chrId <<< filterMap (bedToFeature cs))


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


geneRenderer :: Renderer BedFeature
geneRenderer =
  inj _single
      { draw: bedDraw
      , horPlace
      , verPlace: const (Normalized 0.10) }



fetchJsonChunks :: String
                -> Aff _ (Producer (Array Json) (Aff _) Unit)
fetchJsonChunks url = do
  json <- _.response <$> Affjax.get url

  case json ^? _Array of
    Nothing -> throwError $ error "Parse error: JSON is not an array"
    Just ls -> pure $ chunkProducer 512 ls


featureProd :: forall r1 r2.
               String
            -> (Json -> Maybe { feature :: { chrId :: ChrId | r1 } | r2 })
            -> Aff _
                 (Producer
                 (Map ChrId (Array { feature :: { chrId :: ChrId | r1 } | r2 }))
                 (Aff _) Unit)
featureProd url parse = do
  prod <- fetchJsonChunks url
  pure $ prod $~ (transform $ groupToChrs _.feature.chrId <<< filterMap parse)


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


fetchJSON :: forall a.
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


gwasDraw :: Color
         -> Drawing
gwasDraw color =
  let r = 2.2
      c = circle 0.0 0.0 r
      out = outlined (outlineColor color) c
      fill = filled (fillColor color) c
  in out <> fill

scoreVerPlace :: forall r1 r2.
                  { min :: Number, max :: Number | r1 }
               -> Feature {score :: Number | r2}
               -> Normalized Number
scoreVerPlace s x = Normalized $ (x.feature.score - s.min) / (s.max - s.min)


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
  score <-           obj ^? ix "af"  <<< _Number
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
getGWAS cs url = groupToChrs _.feature.chrId
                  <$> fetchJSON (gemmaJSONParse cs) url


getAnnotations :: CoordSys ChrId BigInt -> String
               -> Aff _ (Map ChrId (Array (Annot ())))
getAnnotations cs url = groupToChrs _.feature.chrId
                        <$> fetchAnnotJSON cs url



pointRenderer :: forall r1 r2.
                 { min :: Number, max :: Number, sig :: Number | r2 }
              -> (Feature {score :: Number | r1} -> DrawingV)
              -> Renderer (Feature {score :: Number | r1})
pointRenderer s draw = inj _single { horPlace, verPlace: scoreVerPlace s, draw }


batchPointRenderer :: forall r1 r2.
                      { min :: Number, max :: Number, sig :: Number | r2 }
                   -> Drawing
                   -> Renderer (Feature {score :: Number | r1})
batchPointRenderer s drawing = inj _batch { drawing, place  }
  where place :: _
        place f = let (Normalized y) = scoreVerPlace s f
                      (Normalized x) = lhs $ horPlace f
                  in Normalized {x,y}
        lhs = case_ # onMatch
                      { point: (\x -> x)
                      , range: (\(Pair l _) -> l) }


-- TODO Configgable Annotation -> LegendEntry function (somehow?!)
annotLegendEntry :: forall r. Annot r -> LegendEntry
annotLegendEntry a =
  if (String.length a.feature.name) `mod` 2 == 0
    then mkIcon blue "even name"
    else mkIcon red  "odd name"


annotLegendTest :: forall f r.
                   Foldable f
                => Functor f
                => Map ChrId (f (Annot r))
                -> Array LegendEntry
annotLegendTest fs = trackLegend annotLegendEntry as
  where as = Array.concat
             $ Array.fromFoldable
             $ Array.fromFoldable <$> Map.values fs


annotDraw :: forall r.
             Annot r
          -> DrawingV
annotDraw an = inj _point $ (lg.icon) <> text'
  where lg = annotLegendEntry an
        text' = Drawing.text
                  (font sansSerif 12 mempty)
                  (7.5) (2.5)
                  (fillColor black) an.feature.name


demoTracks :: VScale
           -> { gwas        :: Maybe (Map ChrId (Array (GWASFeature ()         )))
              , annotations :: Maybe (Map ChrId (Array (Annot (score :: Number))))
              , genes       :: Maybe (Map ChrId (Array BedFeature)) }
           -> List (Exists Track)
demoTracks vs {gwas, annotations, genes} =
  let mkGwas   = Track $ batchPointRenderer vs (gwasDraw navy)
      mkAnnots = Track $ pointRenderer vs annotDraw
      mkGenes  = Track $ geneRenderer
  in List.fromFoldable $ filtered
     [ mkExists <$> mkGwas   <$> gwas
     , mkExists <$> mkAnnots <$> annotations
     , mkExists <$> mkGenes  <$> genes
     ]





------------ new renderers~~~~~~~~~


placeScored :: forall r1 r2.
            { min :: Number, max :: Number | r1 }
         -> Feature { score :: Number | r2 }
         -> NPoint
placeScored {min, max} { frameSize, position: (Pair l _), feature } = {x, y}
  where x = Normalized $ unwrap $ l / frameSize
        y = Normalized $ (feature.score - min) / (max - min)


dist :: Point -> Point -> Number
dist p1 p2 = Math.sqrt $ x' `Math.pow` 2.0 + y' `Math.pow` 2.0
  where x' = p1.x - p2.x
        y' = p1.y - p2.y




renderGWAS :: forall r.
             { min :: Number, max :: Number | r }
          -> Canvas.Dimensions
          -> Map ChrId (Array (GWASFeature ()))
          -> Map ChrId (Pair Number)
          -> Rendered (GWASFeature ())
renderGWAS verscale cdim snps =
  let features :: Array (GWASFeature ())
      features = fold snps

      radius = 2.2

      drawing =
          let color = navy
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
              covers (Tuple f fPt) =
                if dist fPt pt <= radius + radius' then Just f else Nothing


  in \seg -> let pts = pointed seg
             in { features
                , drawings: drawings pts
                , overlaps: overlaps pts }



renderAnnot :: forall r.
             { min :: Number, max :: Number | r }
          -> Canvas.Dimensions
          -> Map ChrId (Array (Annot (score :: Number)))
          -> Map ChrId (Pair Number)
          -> Rendered (Annot (score :: Number))
renderAnnot verscale cdim annots =
  let features :: Array (Annot (score :: Number))
      features = fold annots

      drawing :: Tuple (Annot (score :: Number)) Point -> DrawingN
      drawing (Tuple an pt) = { drawing: lg.icon <> text', points: [pt] }
        where lg = annotLegendEntry an
              text' = Drawing.text
                          (font sansSerif 12 mempty)
                          (7.5) (2.5)
                          (fillColor black) an.feature.name

      drawings :: Array (Tuple (Annot (score :: Number)) Point) -> Array DrawingN
      drawings = map drawing

      npointed :: Map ChrId (Array (Tuple (Annot (score :: Number)) NPoint))
      npointed = (map <<< map) (fanout id (placeScored verscale)) annots

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
                , overlaps: overlaps  }
