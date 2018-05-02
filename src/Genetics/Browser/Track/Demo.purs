module Genetics.Browser.Track.Demo where

import Prelude

import Color (Color, black)
import Color.Scheme.Clrs (blue, red)
import Color.Scheme.X11 (darkblue, darkgrey, lightgrey)
import Control.Coroutine (Producer, transform, ($~), (~~))
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except (runExcept)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap, partition, partitionMap)
import Data.Foldable (class Foldable, fold, foldMap, foldr, length, maximumBy, minimumBy, sequence_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foreign (F, Foreign, ForeignError(..))
import Data.Foreign (readArray) as Foreign
import Data.Foreign.Index (readProp) as Foreign
import Data.Foreign.Keys (keys) as Foreign
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (view, (^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap, wrap)
import Data.Pair (Pair(..))
import Data.Pair as Pair
import Data.Profunctor.Strong (fanout)
import Data.Record.Builder (build, insert, modify)
import Data.Record.Extra (class Keys)
import Data.Record.Extra (keys) as Record
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple as Tuple
import Data.Unfoldable (unfoldr)
import Data.Variant (inj)
import Debug.Trace as Debug
import Genetics.Browser.Track.Backend (DrawingN, DrawingV, Feature, Label, LegendEntry, NPoint, OldRenderer, RenderedTrack, featureNormX, groupToMap, mkIcon, trackLegend)
import Genetics.Browser.Track.Bed (ParsedLine, chunkProducer, fetchBed, fetchForeignChunks, parsedLineTransformer)
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId), NegLog10(..), _NegLog10)
import Genetics.Browser.Types.Coordinates (CoordSys, Normalized(Normalized), _Segments, aroundPair, normalize, pairSize, pairsOverlap)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Point, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Math as Math
import Network.HTTP.Affjax as Affjax
import Simple.JSON as Simple
import Type.Prelude (class RowToList, SProxy(SProxy))
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

  pure $ groupToMap _.feature.chrId $ fs


bedDraw :: BedFeature
        -> DrawingV
bedDraw gene = inj (SProxy :: SProxy "range") \w ->
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
geneRenderer = inj (SProxy :: SProxy "single") { draw: bedDraw, horPlace, verPlace: const (Normalized 0.10) }
  where horPlace {position, frameSize} =
          let f p = Normalized (unwrap $ p / frameSize)
          in inj (SProxy :: SProxy "range") $ map f position



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



type SNPRow chr score r =
  ( "chrId"  :: chr
  , "name" :: String
  , "score" :: score
  | r )

type SNP r =
  Feature (Record (SNPRow ChrId Number r))

parseSNP :: CoordSys ChrId BigInt
         -> Foreign
         -> F (SNP ())
parseSNP cSys a = do
  raw <- Simple.read' a

  let
      snp :: { chr :: _, ps :: _, p_wald :: _, rs :: _ }
      snp = raw

      feature :: Record (SNPRow ChrId Number ())
      feature =
        { score:      snp.p_wald
        , chrId: wrap snp.chr
        , name:       snp.rs }

      position = (\p -> wrap <$> Pair p p) $ snp.ps

  frameSize <- case Map.lookup feature.chrId (view _Segments cSys) of

    Nothing ->
      throwError $ pure $ ForeignError
                     "Annotation chr not found in coordinate system!"
    Just seg ->
      pure $ Bp $ BigInt.toNumber $ pairSize seg

  pure $ { position
         , frameSize
         , feature }



type AnnotationRow chr pos r =
  ( "chr"  :: chr
  , "pos"  :: pos
  , "name" :: String
  , "url"  :: Maybe String
  , "gene" :: Maybe String
  | r )

type AnnotationField = { field :: String, value :: Foreign }

type Annotation r =
  Feature (Record (AnnotationRow ChrId Bp
                     ( rest :: List AnnotationField | r )))

annotationFields :: ∀ a b rl.
                    Keys rl
                 => RowToList (AnnotationRow a b ()) rl
                 => List String
annotationFields =
  Record.keys (unsafeCoerce unit :: Record (AnnotationRow a b ()))


parseAnnotation :: CoordSys ChrId BigInt
                -> Foreign
                -> F (Annotation ())
parseAnnotation cSys a = do
  raw <- Simple.read' a
  rest <- parseAnnotationRest a

  let
      feature =
        build
         (insert    (SProxy :: SProxy "rest") rest
         >>> modify (SProxy :: SProxy "chr") ChrId
         >>> modify (SProxy :: SProxy "pos") Bp) raw

      position = (\p -> Pair p p) $ feature.pos

  frameSize <- case Map.lookup feature.chr (view _Segments cSys) of


    Nothing ->
      throwError $ pure $ ForeignError
                     "Annotation chr not found in coordinate system!"
    Just seg ->
      pure $ Bp $ BigInt.toNumber $ pairSize seg

  pure $ { position
         , frameSize
         , feature }


-- | Partially parse the parts of the annotation record that are *not* in the Annotation type
parseAnnotationRest :: Foreign
                    -> F (List AnnotationField)
parseAnnotationRest a = do
  allFields <- List.fromFoldable <$> Foreign.keys a

  let restFields = allFields `List.difference` annotationFields

  for restFields \field ->
     { field, value: _ } <$> Foreign.readProp field a


-- | A completely dumb default way of rendering arbitrary annotation fields
showAnnotationField :: AnnotationField -> String
showAnnotationField fv = fv.field <> ": " <> unsafeCoerce fv.value


showAnnotation :: Annotation ()
               -> List String
showAnnotation a = (List.fromFoldable
                    [ name, chr, pos ]) <> (map showAnnotationField annot.rest)
  where annot = a.feature
        name = fromMaybe ("SNP: " <> annot.name)
                         (("Gene: " <> _) <$> annot.gene)
        chr = "Chr: " <> show annot.chr
        pos = "Pos: " <> show annot.pos


getSNPs :: CoordSys ChrId BigInt
        -> String
        -> Aff _ (Map ChrId (Array (SNP ())))
getSNPs cs url = do
  resp <- Affjax.get url

  rawSNPs <-
    case runExcept $ Foreign.readArray resp.response of
           Left err -> throwError $ error "SNP data is not an array"
           Right as -> pure as

  let parsed =
        partitionMap (runExcept <<< parseSNP cs) rawSNPs

  pure $ groupToMap _.feature.chrId
         $ filter (\f -> Pair.fst (f.position) >= wrap zero)
         $ parsed.right


getAnnotations :: CoordSys ChrId BigInt
               -> String
               -> Aff _ (Map ChrId (Array (Annotation ())))
getAnnotations cs url = do
  resp <- Affjax.get url

  rawAnnots <- case runExcept
                    $ Foreign.readArray resp.response of
                 Left err -> throwError $ error "Annotations data is not an array"
                 Right as -> pure as

  let parsed = partitionMap
               (runExcept <<< parseAnnotation cs) rawAnnots


  -- debug/testing stuff
  liftEff do
    log $ "Raw annotations array length: " <> show (Array.length rawAnnots)
    log $ "Could not parse "
        <> show (length parsed.left :: Int)
        <> " annotations."
    log $ "Successfully parsed "
        <> show (length parsed.right :: Int)
        <> " annotations."

    -- logging with unsafeCoerce lets us examine the raw JS object in the console!
    log $ unsafeCoerce (parsed.right)
  case Array.head parsed.right of
    Nothing -> pure unit
    Just an -> liftEff do
      log "first annotation: "
      sequence_ (log <$> (("> " <> _) <$> showAnnotation an))

  pure $ groupToMap _.feature.chr parsed.right



type Peak' x r = { covers :: Pair x
                 , peak   :: r
                 , mountain  :: Array r }


peak' :: ∀ x y a.
         Ord x
      => Ring x
      => { pos   :: a -> Pair x
         , value :: a -> Ordering }
      -> x
      -> Array a
      -> Maybe (Tuple (Peak' x a)
                      (Array a))
peak' {pos, value} radius as = do
  peak <- maximumBy (compare `on` value) as

  let covers = radius `aroundPair` pos peak
      {no, yes} = partition (\p -> pos p `pairsOverlap` covers) as

  pure $ Tuple {covers, peak, mountain: yes} no





type Peak x y r = { covers :: Pair x
                  , y :: y
                  , elements :: Array r }

peak1 :: ∀ rS.
         Bp
      -> Array (SNP rS)
      -> Maybe (Tuple (Peak _ _ (SNP rS))
                      (Array (SNP rS)))
peak1 radius snps = do
  top <- minimumBy (compare `on` _.feature.score) snps

  let covers = radius `aroundPair` top.position
      y = top.feature.score
      {no, yes} = partition (\p -> p.position `pairsOverlap` covers) snps

  pure $ Tuple {covers, y, elements: yes} no


peaks :: ∀ rS.
         Bp
      -> Array (SNP rS)
      -> Array (Peak Bp Number (SNP rS))
peaks r snps = unfoldr (peak1 r) snps



annotationLegendEntry :: ∀ r. Annotation r -> LegendEntry
annotationLegendEntry a =
  case a.feature.gene of
    Nothing -> mkIcon blue "SNP name"
    Just gn -> mkIcon red "Gene name"


annotationLegendTest :: ∀ f r.
                   Foldable f
                => Functor f
                => Map ChrId (f (Annotation r))
                -> Array LegendEntry
annotationLegendTest fs = trackLegend annotationLegendEntry as
  where as = Array.concat
             $ Array.fromFoldable
             $ Array.fromFoldable <$> Map.values fs






------------ new renderers~~~~~~~~~


placeScored :: ∀ r1 r2.
               { min :: Number, max :: Number | r1 }
            -> Feature { score :: Number | r2 }
            -> NPoint
placeScored verScale { frameSize, position: (Pair l _), feature } = {x, y}
  where x = Normalized $ unwrap $ l / frameSize
        y = normalizedY verScale feature.score


normalizedY :: ∀ r.
               { min :: Number, max :: Number | r }
            -> Number
            -> Normalized Number
normalizedY {min, max} y = Normalized $ ((negLog10 y) - min) / (max - min)


dist :: Point -> Point -> Number
dist p1 p2 = Math.sqrt $ x' `Math.pow` 2.0 + y' `Math.pow` 2.0
  where x' = p1.x - p2.x
        y' = p1.y - p2.y



filterSig :: ∀ r1 r2.
             { sig :: Number | r1 }
filterSig {sig} = map (filter isSignificant)
  where sig' = 10.0 `Math.pow` (-sig)
        isSignificant {feature} = feature.score <= sig'

          -> Map ChrId (Array (SNP r2))
          -> Map ChrId (Array (SNP r2))



renderSNPs :: ∀ r.
              { min :: Number, max :: Number | r }
           -> Canvas.Dimensions
           -> Map ChrId (Array (SNP ()))
           -> Map ChrId (Pair Number)
           -> RenderedTrack (SNP ())
renderSNPs verScale cdim snps =
  let features :: Array (SNP ())
      features = fold snps

      radius = 3.75

      drawing =
          let color = darkblue
              c = circle 0.0 0.0 radius
              out = outlined (outlineColor color) c
              fill = filled (fillColor color) c
          in out <> fill

      drawings :: Array (Tuple (SNP ()) Point) -> Array DrawingN
      drawings pts = let (Tuple _ points) = Array.unzip pts
                     in [{ drawing, points }]

      npointed = (map <<< map) (fanout id (placeScored verscale)) snps
      npointed :: Map ChrId (Array (Tuple (SNP ()) NPoint))

      rescale :: Pair Number -> NPoint -> Point
      rescale seg npoint =
        let (Pair offset _) = seg
            x = offset + (pairSize seg) * (unwrap npoint.x)
            y = cdim.height * (one - unwrap npoint.y)
        in {x, y}


      pointed :: Map ChrId (Pair Number)
              -> Array (Tuple (SNP ()) Point)
      pointed segs = foldMapWithIndex f segs
        where f chrId seg = foldMap ((map <<< map) $ rescale seg)
                              $ Map.lookup chrId npointed

      overlaps :: Array (Tuple (SNP ()) Point)
               -> Number -> Point
               -> Array (SNP ())
      overlaps pts radius' pt = filterMap covers pts
        where covers :: Tuple (SNP ()) Point -> Maybe (SNP ())
              covers (Tuple f fPt)
                | dist fPt pt <= radius + radius'   = Just f
                | otherwise = Nothing

  in \seg -> let pts = pointed seg
             in { features
                , drawings: drawings pts
                , labels: mempty
                , overlaps: overlaps pts }


annotationForSnp :: ∀ rA rS.
               Bp
            -> Map ChrId (Array (Annotation rA))
            -> GWASFeature rS
            -> Maybe (Annotation rA)
annotationForSnp radius annotations {position, feature} = do
  chr <- Map.lookup feature.chrId annotations
  Array.find (\a -> ((radius `aroundPair` a.position) `pairsOverlap` position)) chr






renderAnnotation :: ∀ r r1 r2.
                    CoordSys _ _
                 -> Map ChrId (Array (SNP ()))
                 -> { min :: Number, max :: Number | r }
                 -> Canvas.Dimensions
                 -> Map ChrId (Array (Annotation r2))
                 -> Map ChrId (Pair Number)
                 -> RenderedTrack (Annotation r2)
renderAnnotation cSys sigSnps verscale cdim allAnnots =
  let features :: Array (Annotation r2)
      features = fold allAnnots

      curAnnotPeaks :: Map ChrId (Pair Number)
                    -> Map ChrId (Array (Peak Number Number (Annotation r2)))
      curAnnotPeaks = mapWithIndex f
        where f :: ChrId -> Pair Number -> Array (Peak _ _ (Annotation r2))
              f chr seg = fromMaybe [] do
                  snps <- Map.lookup chr sigSnps
                  segSize <- _.frameSize <$> Array.head snps

                  let rad = ((wrap 3.75) * segSize) / (wrap $ pairSize seg)
                      snpPeaks = Debug.trace ("radius: " <> show rad) \_ -> peaks rad snps

                      peakAnnots pk = { covers, y, elements }
                        where offset = Pair.fst seg
                              covers = map (\x -> offset + pairSize seg * (unwrap $ x / segSize)) pk.covers
                              y = cdim.height * (one - (unwrap $ normalizedY verscale pk.y))
                              elements = fromMaybe [] do
                                chr <- Map.lookup chr allAnnots
                                pure $ filter (\a -> a.position
                                                  `pairsOverlap` pk.covers) chr

                  pure $ map peakAnnots snpPeaks



      tailHeight = 0.06
      tailPixels = tailHeight * cdim.height
      iconYOffset = -6.5
      labelOffset = -8.0

      drawing :: Peak Number Number (Annotation r2)
              -> DrawingN
      drawing aPeak = { drawing: tail <> icons
                      , points: [{x, y: aPeak.y }] }
        where (Pair l r) = aPeak.covers
              x = l + 0.5 * (r - l)
              icon' = _.icon <<< annotationLegendEntry
              icons = Drawing.translate 0.0 (-tailPixels)
                      $ foldr (\a d -> Drawing.translate 0.0 iconYOffset
                                     $ icon' a <> d) mempty aPeak.elements

              tail = if Array.null aPeak.elements
                       then mempty
                       else Drawing.outlined (lineWidth 1.3 <> outlineColor black)
                              $ Drawing.path
                                [ {x: 0.0, y: 0.0 }
                                , {x: 0.0, y: -tailPixels }]


      drawings :: Map ChrId (Array (Peak Number Number (Annotation r2)))
               -> Array DrawingN
      drawings = foldMap (map drawing)


      label :: Peak Number Number (Annotation r2) -> Array Label
      label aPeak = Tuple.snd
                    $ foldr f (Tuple y0 mempty) aPeak.elements
        where y0 = aPeak.y - tailPixels + (4.0 * iconYOffset)
              (Pair l r) = aPeak.covers
              x = l + 0.5 * (r - l)
              f a (Tuple y ls) = Tuple (y + labelOffset)
                                    $ Array.snoc ls -- TODO this use of snoc is probably reaaaal slow
                                         { text: fromMaybe a.feature.name a.feature.gene
                                         , point: { x, y } }


      labels :: Map ChrId (Array (Peak Number Number (Annotation r2)))
             -> Array Label
      labels = foldMap (foldMap label)

      overlaps :: Number -> Point -> Array (Annotation r2)
      overlaps r p = mempty

  in \segs -> let curPeaks = curAnnotPeaks segs
              in { features
                 , drawings: drawings curPeaks
                 , labels: labels curPeaks
                 , overlaps: overlaps  }


