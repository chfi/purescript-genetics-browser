module Genetics.Browser.Demo where

import Prelude

import Affjax (get, printResponseFormatError) as Affjax
import Affjax.ResponseFormat (json) as Affjax
import Color (black)
import Color.Scheme.Clrs (blue, red)
import Color.Scheme.X11 (darkblue, darkgrey, lightgrey)
import Control.Monad.Except (runExcept, throwError)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left), either)
import Data.Filterable (filter, filterMap, partition, partitionMap)
import Data.Foldable (class Foldable, foldMap, foldr, length, minimumBy, sequence_, traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (re, view, (^.))
import Data.Lens.Lens.Tuple (_2)
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Pair (Pair(..))
import Data.Pair as Pair
import Data.Profunctor.Strong (fanout)
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple as Tuple
import Data.Unfoldable (unfoldr)
import Data.Variant (inj)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (F, Foreign, ForeignError(ForeignError))
import Foreign (readArray) as Foreign
import Foreign.Index (readProp) as Foreign
import Foreign.Keys (keys) as Foreign
import Genetics.Browser (Feature, HexColor, LegendConfig, LegendEntry, Peak, Threshold, VScale, VScaleRow, chrBackgroundLayer, chrLabelsLayer, drawLegendInSlot, drawVScaleInSlot, featureNormX, groupToMap, fixedUILayer, thresholdRuler, trackLegend, trackLikeLayer)
import Genetics.Browser.Bed (BedFeature)
import Genetics.Browser.Canvas (BatchDrawing, BigDrawing, Label, LabelPlace(LLeft, LCenter), Renderable, TrackContainer, _bigDrawing, _drawingBatch, _labels, newLayer)
import Genetics.Browser.Coordinates (CoordSys, CoordSysView, _Segments, aroundPair, normalize, pairSize, pairsOverlap)
import Genetics.Browser.Layer (Component(..), Layer(..), LayerMask(..), LayerType(..), TrackPadding)
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId), NegLog10(..), _NegLog10)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Point, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle)
import Graphics.Drawing as Drawing
import Math as Math
import Record (insert) as Record
import Record.Builder (build, insert, merge, modify)
import Record.Extra (class Keys)
import Record.Extra (keys) as Record
import Simple.JSON as Simple
import Type.Prelude (class RowToList, SProxy(SProxy))
import Unsafe.Coerce (unsafeCoerce)


renderGenes :: Map ChrId (Array BedFeature)
            -> _
            -> Map ChrId (Pair Number)
            -> Canvas.Dimensions
            -> { renderables :: List Renderable }
renderGenes geneData _ segs size =
  let
      geneData' :: List (Tuple ChrId (List BedFeature))
      geneData' = Map.toUnfoldable
              $ map List.fromFoldable
              $ geneData

      glyphs :: List {bigDrawing :: _, label :: _}
      glyphs = List.concatMap f geneData'
        where f :: Tuple ChrId (List BedFeature) -> _
              f (Tuple chr genes) = fromMaybe mempty do
                  seg <- Map.lookup chr segs
                  pure $ filterMap (drawGene size seg) genes

      drawings :: List Renderable
      drawings = map (inj _bigDrawing <<< _.bigDrawing) glyphs
      labels :: Renderable
      labels = inj _labels $ map _.label glyphs

  in { renderables: labels : drawings }


drawGene :: Canvas.Dimensions
         -> Pair Number
         -> BedFeature
         -> Maybe { bigDrawing :: BigDrawing, label :: Label }
drawGene size seg gene =
  let (Pair l r) = gene.position
      segWidth = pairSize seg
      toLocal x = segWidth * (unwrap $ x / gene.frameSize)

      width = toLocal (r - l)

      exon block =
        let p@(Pair exL' size') = toLocal <$> block
        in rectangle exL' zero size' 30.0

      exonSize b = Pair.snd $ toLocal <$> b

      exons _ = let s' = foldMap exon $ filter (\b -> exonSize b > one) gene.feature.blocks
                in (outlined (outlineColor darkgrey <> lineWidth 1.0) s')
                   <> (filled (fillColor lightgrey) s')

      introns _ =
        let s = rectangle 1.5 14.0 (width-1.5) 2.0
        in outlined (outlineColor black <> lineWidth 3.0) s
        <> filled   (fillColor black) s

      drawing _ =
          introns unit
          <> exons unit


  in if width < one
     then Nothing
     else let point :: Point
              point =
                let (Pair offset _) = seg
                in { x: offset + (pairSize seg) * (unwrap $ featureNormX gene)
                   , y: size.height * 0.5 }

              topLeft = point
              bottomRight = { x: point.x + width
                            , y: point.y + 15.0 }

          in pure { bigDrawing: { drawing
                                , topLeft
                                , bottomRight
                                }
                  , label:   { text: gene.feature.geneName
                             , point
                             , gravity: LCenter }  }



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
      snp :: { chr :: String, ps :: Number, p_wald :: Number, rs :: String }
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
        -> Aff (Map ChrId (Array (SNP ())))
getSNPs cs url = do
  resp <- Affjax.get Affjax.json url

  liftEffect $ log $ unsafeCoerce resp.body

  body <- either (throwError
                  <<< error
                  <<< Affjax.printResponseFormatError)
          pure resp.body

  rawSNPs <-
    case runExcept
         $ Foreign.readArray
         $ unsafeCoerce body of
           Left err -> throwError $ error "SNP data is not an array"
           Right as -> pure as


  let parsed =
        partitionMap (runExcept <<< parseSNP cs) rawSNPs

  pure $ groupToMap _.feature.chrId
         $ filter (\f -> Pair.fst (f.position) >= wrap zero)
         $ parsed.right


getAnnotations :: CoordSys ChrId BigInt
               -> String
               -> Aff (Map ChrId (Array (Annotation ())))
getAnnotations cs url = do
  resp <- Affjax.get Affjax.json url

  body <- either (throwError
                  <<< error
                  <<< Affjax.printResponseFormatError)
          pure resp.body

  rawAnnots <- case runExcept
                    $ Foreign.readArray
                    $ unsafeCoerce body of
                 Left err -> throwError $ error "Annotations data is not an array"
                 Right as -> pure as

  let parsed = partitionMap
               (runExcept <<< parseAnnotation cs) rawAnnots


  -- debug/testing stuff
  liftEffect do
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
    Just an -> liftEffect do
      log "first annotation: "
      sequence_ (log <$> (("> " <> _) <$> showAnnotation an))

  pure $ groupToMap _.feature.chr parsed.right


snpPeak :: ∀ rS.
           Bp
        -> Array (SNP rS)
        -> Maybe (Tuple (Peak Bp Number (SNP rS))
                        (Array (SNP rS)))
snpPeak radius snps = do
  top <- minimumBy (compare `on` _.feature.score) snps

  let covers = radius `aroundPair` top.position
      y = top.feature.score
      {no, yes} = partition (\p -> p.position `pairsOverlap` covers) snps

  pure $ Tuple {covers, y, elements: yes} no



peak1 :: ∀ rS.
         Bp
      -> Array (SNP rS)
      -> Maybe (Tuple (Peak Bp Number (SNP rS))
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

type AnnotationsConfig =
  { radius    :: Number
  , outline   :: HexColor
  , snpColor  :: HexColor
  , geneColor :: HexColor }

defaultAnnotationsConfig :: AnnotationsConfig
defaultAnnotationsConfig =
  { radius:    5.5
  , outline:   wrap black
  , snpColor:  wrap blue
  , geneColor: wrap red }

annotationLegendEntry :: ∀ r. AnnotationsConfig -> Annotation r -> LegendEntry
annotationLegendEntry conf a =
  let mkIcon color = outlined (outlineColor (unwrap conf.outline) <> lineWidth 2.0)
                  <> filled (fillColor $ unwrap color)
                   $ circle 0.0 0.0 conf.radius
  in case a.feature.gene of
       Nothing -> { text: "SNP name",  icon: mkIcon conf.snpColor  }
       Just gn -> { text: "Gene name", icon: mkIcon conf.geneColor }


annotationLegendTest :: ∀ f r.
                   Foldable f
                => Functor f
                => AnnotationsConfig
                -> Map ChrId (f (Annotation r))
                -> Array LegendEntry
annotationLegendTest config fs = trackLegend (annotationLegendEntry config) as
  where as = Array.concat
             $ Array.fromFoldable
             $ Array.fromFoldable <$> Map.values fs


normYLogScore :: ∀ r.
                 { min :: Number
                 , max :: Number
                 | r }
              -> Number
              -> Number
normYLogScore s =
  normalize s.min s.max <<< unwrap <<< view _NegLog10


dist :: Point -> Point -> Number
dist p1 p2 = Math.sqrt $ x' `Math.pow` 2.0 + y' `Math.pow` 2.0
  where x' = p1.x - p2.x
        y' = p1.y - p2.y


filterSig :: ∀ r1 r2.
             { sig :: Number | r1 }
          -> Map ChrId (Array (SNP r2))
          -> Map ChrId (Array (SNP r2))
filterSig {sig} = map (filter
  (\snp -> snp.feature.score <= (NegLog10 sig ^. re _NegLog10)))



snpsUI :: _
snpsUI vscale = fixedUILayer (CLeft $ drawVScaleInSlot vscale)


annotationsUI :: _
annotationsUI legend = fixedUILayer (CRight $ drawLegendInSlot legend)



type SNPConfig
  = { radius :: Number
    , lineWidth :: Number
    , color :: { outline :: HexColor
               , fill    :: HexColor }
    , pixelOffset :: Point }

defaultSNPConfig :: SNPConfig
defaultSNPConfig =
  { radius: 3.75
  , lineWidth: 1.0
  , color: { outline: wrap darkblue, fill: wrap darkblue }
  , pixelOffset: {x: 0.0, y: 0.0}
  }


renderSNPs :: ∀ r1.
              Map ChrId (Array (SNP ()))
           -> { threshold  :: { min  :: Number, max :: Number | r1 }
              , render :: SNPConfig }
           -> Map ChrId (Pair Number)
           -> Canvas.Dimensions
           -> { renderables :: List Renderable
              , hotspots :: Number -> Point -> Array (SNP ()) }
renderSNPs snpData { render, threshold } =
  let radius = render.radius

      snpData' = List.fromFoldable <$> snpData

      drawing =
          let {x,y} = render.pixelOffset
              c     = circle x y radius
              out   = outlined (outlineColor (unwrap render.color.outline) <>
                                lineWidth render.lineWidth) c
              fill  = filled   (fillColor    $ unwrap render.color.fill)    c
          in out <> fill

      drawings :: List (Tuple (SNP ()) Point) -> Array BatchDrawing
      drawings pts = pure { drawing, points: view _2 <$> (Array.fromFoldable pts) }


      pointed :: Canvas.Dimensions
              -> Map ChrId (Pair Number)
              -> List (Tuple (SNP ()) Point)
      pointed size = foldMapWithIndex placeSeg
        where
              place :: Pair Number -> SNP () -> Point
              place seg@(Pair offset _) s =
                { x: offset + (pairSize seg) * (unwrap $ featureNormX s)
                , y: size.height * (one - normYLogScore threshold s.feature.score) }

              placeSeg :: ChrId -> Pair Number -> List (Tuple (SNP ()) Point)
              placeSeg chrId seg = foldMap (map $ fanout identity (place seg))
                                    $ Map.lookup chrId snpData'


      hotspots :: List (Tuple (SNP ()) Point)
               -> Number -> Point
               -> Array (SNP ())
      hotspots pts radius' pt = Array.fromFoldable $ filterMap covers pts
        where covers :: Tuple (SNP ()) Point -> Maybe (SNP ())
              covers (Tuple f fPt)
                | dist fPt pt <= radius + radius'   = Just f
                | otherwise = Nothing

  in \seg size ->
        let pts = pointed size seg
        in { renderables: pure $ inj _drawingBatch $ drawings pts
           , hotspots: hotspots pts }



annotationsForScale :: ∀ r1 r2 i c.
                       CoordSys i c
                    -> Map ChrId (Array (SNP r1))
                    -> Map ChrId (Array (Annotation r2))
                    -> Map ChrId (Pair Number)
                    -> Map ChrId (Array (Peak Bp Number (Annotation r2)))
annotationsForScale cSys snps annots =
  mapWithIndex \chr seg -> fromMaybe [] do
    segSnps      <- Map.lookup chr snps
    segAnnots    <- Map.lookup chr annots
    frameSize <- _.frameSize <$> Array.head segSnps

    let rad = (wrap 3.75 * frameSize) / (wrap $ pairSize seg)
        f pk = pk { elements = filter
                          (pairsOverlap pk.covers <<< _.position) segAnnots }

    pure $ f <$> peaks rad segSnps


renderAnnotationPeaks :: ∀ r1 r2.
                         CoordSys ChrId BigInt
                      -> { min :: Number, max :: Number | r1 }
                      -> AnnotationsConfig
                      -> Map ChrId (Array (Peak Bp Number (Annotation r2)))
                      -> Canvas.Dimensions
                      -> Map ChrId (Pair Number)
                      -> { drawingBatch :: Array BatchDrawing
                         , labels :: List Label }
renderAnnotationPeaks cSys vScale conf annoPks cdim =
  let

      curAnnotPeaks :: Map ChrId (Pair Number)
                    -> Map ChrId (Array (Peak Number Number (Annotation r2)))
      curAnnotPeaks segs = mapWithIndex f annoPks
        where f chr pks = fromMaybe [] do
                  frameSize <- (wrap <<< BigInt.toNumber <<< pairSize)
                             <$> (Map.lookup chr $ cSys ^. _Segments)
                  pxSeg@(Pair l _) <- Map.lookup chr segs

                  let rescale pk = pk { covers = (\x -> l + pairSize pxSeg *
                                          (unwrap $ x / frameSize)) <$> pk.covers
                                      , y = cdim.height * (one - normYLogScore vScale pk.y) }

                  pure $ map rescale pks


      tailHeight = 0.06
      tailPixels = tailHeight * cdim.height
      iconYOffset = 6.5
      labelOffset = 8.0

      -- part of the canvas that a given peak will cover when rendered
      drawingCovers :: ∀ a.
                       Peak Number Number a
                    -> Canvas.Rectangle
      drawingCovers aPeak = {x, y, width: w, height: h}
        where (Pair x _) = aPeak.covers
              w = 14.0  -- hardcoded glyph width in pixels
              gH = (11.0 - iconYOffset) -- hardcoded height
              h = tailPixels + gH * length aPeak.elements
              y = aPeak.y - h


      drawAndLabel :: Peak Number Number (Annotation r2)
                   -> Tuple (Array BatchDrawing) (List Label)
      drawAndLabel aPeak = Tuple [drawing] label
        where
              (Pair l r) = aPeak.covers
              x = l + 0.5 * (r - l)

              icons = Drawing.translate 0.0 (-tailPixels)
                      $ foldr (\a d -> Drawing.translate 0.0 (-iconYOffset)
                                     $ (annotationLegendEntry conf a).icon <> d)
                          mempty aPeak.elements

              tail = if Array.null aPeak.elements
                then mempty
                else Drawing.outlined (lineWidth 1.3 <> outlineColor black)
                       $ Drawing.path [ {x: 0.0, y: 0.0 }, {x: 0.0, y: -tailPixels }]

              drawing = { drawing: tail <> icons, points: [{x, y: aPeak.y }] }

              -- hardcoded font size because this will be handled by UI.Canvas later
              fontHeight = 14.0
              labelsHeight = length aPeak.elements * fontHeight

              dC = drawingCovers aPeak
                -- if all the labels fit above the icons, they're centered;
                -- otherwise just place them to the left and hope for the best
              {g, y0} = if dC.y > labelsHeight
                then { g: LCenter, y0: dC.y  }
                else { g: LLeft, y0: aPeak.y }

              label = Tuple.snd $ foldr f (Tuple y0 mempty) aPeak.elements
              f a (Tuple y ls) = Tuple (y - labelOffset)
                                    $ List.Cons { text: fromMaybe a.feature.name a.feature.gene
                                                , point: { x, y }
                                                , gravity: g
                                                } ls


      drawAndLabelAll :: Map ChrId (Array (Peak _ _ _))
                      -> Tuple (Array BatchDrawing) (List Label)
      drawAndLabelAll pks = foldMap (foldMap drawAndLabel) pks


  in \segs -> let (Tuple drawingBatch labels) =
                    drawAndLabelAll $ curAnnotPeaks segs
              in { drawingBatch, labels }


renderAnnotations :: ∀ r1 r2 rC.
                     CoordSys ChrId BigInt
                  -> Map ChrId (Array (SNP ()))
                  -> Map ChrId (Array (Annotation r1))
                  -> { threshold :: { min :: Number, max :: Number | r2 }
                     , render :: AnnotationsConfig | rC }
                  -> Map ChrId (Pair Number)
                  -> Canvas.Dimensions
                  -> { renderables :: List Renderable }
renderAnnotations cSys sigSnps allAnnots conf =
  let annoPeaks = annotationsForScale cSys sigSnps allAnnots

  in \seg size ->
  let {drawingBatch, labels} =
              renderAnnotationPeaks cSys conf.threshold conf.render (annoPeaks seg) size seg
        in { renderables: List.fromFoldable
           [ inj _drawingBatch $ drawingBatch
           , inj _labels   $ labels ] }


_snps = SProxy :: SProxy "snps"
_annotations = SProxy :: SProxy "annotations"


addChrLayers :: ∀ r.
                { coordinateSystem :: CoordSys ChrId BigInt
                , segmentPadding :: Number | r }
             -> { chrLabels :: { fontSize :: Int }
                , chrBG1 :: HexColor
                , chrBG2 :: HexColor }
             -> TrackContainer
             -> Aff (Pair Number -> CoordSysView -> Aff Unit)
addChrLayers trackConf {chrLabels, chrBG1, chrBG2} tc = do

  rBG  <- newLayer tc "chrBackground"
          $ trackLikeLayer trackConf (SProxy :: _ "background")
            (Center chrBackgroundLayer)

  rChrLabels <- newLayer tc "chrLabels"
                $ chrLabelsLayer trackConf { fontSize: chrLabels.fontSize }

  let chrs o v = do
        let background = { chrBG1
                         , chrBG2
                         , segmentPadding: trackConf.segmentPadding }
            bgConf = { background
                     , view: v }
            render = \l -> do
                l.run bgConf
                l.last.renderables >>= l.drawOnCanvas o

        traverse_ render
                    [ rBG, rChrLabels ]


  pure chrs

addGWASLayers :: ∀ r.
                 CoordSys ChrId BigInt
              -> { score :: { min :: Number, max :: Number, sig :: Number }
                 , legend :: LegendConfig ()
                 , vscale :: VScale ()
                 , snps :: SNPConfig
                 , annotations :: AnnotationsConfig
                 , trackHeight :: Number
                 , padding :: TrackPadding }
              -> { snps        :: Map ChrId (Array (SNP ()))
                 , annotations :: Map ChrId (Array (Annotation ())) | r }
              -> TrackContainer
              -> Aff { snps        :: Pair Number -> CoordSysView -> Aff Unit
                     , annotations :: Pair Number -> CoordSysView -> Aff Unit
                     , hotspots    :: Aff (Number -> Point -> Array (SNP ()))
                     , fixedUI     :: Aff Unit }
addGWASLayers coordinateSystem config trackData =
  let threshold = config.score
      vscale = build (merge config.vscale) threshold

      sigSnps = filterSig config.score trackData.snps
      legend = Record.insert (SProxy :: SProxy "entries")
               (annotationLegendTest
                 config.annotations
                 trackData.annotations) config.legend

      segmentPadding = 12.0
      conf = { segmentPadding
             , coordinateSystem }
      cSys = coordinateSystem

      snpLayer :: Layer (Tuple _ Canvas.Dimensions -> {renderables :: _, hotspots :: _})
      snpLayer = trackLikeLayer conf _snps
                    (Center $ renderSNPs trackData.snps)


      annotationLayer :: Layer (Tuple _ _ -> _)
      annotationLayer = trackLikeLayer conf _annotations
          (Center $ renderAnnotations cSys sigSnps trackData.annotations)




  in \bc -> do

    rRuler <-
      newLayer bc "ruler" $ Layer Fixed NoMask (Center thresholdRuler)

    rSNPs <-
      newLayer bc "snps" snpLayer

    rAnnos <-
      newLayer bc "annotations" annotationLayer

    rVScale <-
      newLayer bc "vscale" $ snpsUI vscale

    rLegend <-
      newLayer bc "legend" $ annotationsUI legend


    let fixedUI = do
          let uiConf = { rulerColor: wrap red, threshold }

          let render = \l -> do
                l.run uiConf
                l.last.renderables >>= l.drawOnCanvas (Pair 0.0 0.0)

          traverse_ render [rLegend, rVScale, rRuler]


        snps o v = do
          rSNPs.run { snps: {threshold, render: config.snps }, view: v}
          rSNPs.last.renderables >>= rSNPs.drawOnCanvas o


        hotspots = rSNPs.last.hotspots

        annotations o v = do
          rAnnos.run { annotations: {threshold, render: config.annotations }, view: v}
          rAnnos.last.renderables >>= rAnnos.drawOnCanvas o


    pure { snps
         , annotations
         , hotspots
         , fixedUI
         }



addGeneLayers :: ∀ r.
                 CoordSys ChrId BigInt
              -> { trackHeight :: Number
                 , padding :: TrackPadding }
              -> { genes        :: Map ChrId (Array BedFeature) }
              -> TrackContainer
              -> Aff { genes :: Pair Number -> CoordSysView -> Aff Unit }
addGeneLayers coordinateSystem config trackData tcont = do

  let
      segmentPadding = 12.0
      conf = { segmentPadding
             , coordinateSystem }

      geneLayer =
        trackLikeLayer conf (SProxy :: _ "genes")
          (Center $ renderGenes trackData.genes)

  rGenes <- newLayer tcont "genes" geneLayer

  let genes o v = do
        rGenes.run { genes: {}, view: v}
        rGenes.last.renderables >>= rGenes.drawOnCanvas o


  pure { genes }
