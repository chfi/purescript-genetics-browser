module Genetics.Browser.Track.Demo where

import Prelude

import Color (Color, black, white)
import Color.Scheme.Clrs (aqua, blue, fuchsia, green, lime, maroon, navy, olive, orange, purple, red, teal, yellow)
import Control.Monad.Aff (Aff, throwError)
import Control.Monad.Eff.Exception (error)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Foldable (class Foldable, foldMap, length)
import Data.Lens (to, (^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Pair (Pair(..))
import Data.String as String
import Data.Symbol (SProxy(SProxy))
import Data.Traversable (class Traversable, traverse)
import Data.Variant (Variant, case_, inj, on)
import Genetics.Browser.Track.Backend (BrowserTrack, CanvasBatchGlyph, ChrCtx, DrawingR, DrawingV, Feature, Glyph, Legend, LegendEntry, Padding, Renderer, VScale, VScaleRow, _point, _range, boxesTrack, bumpFeatures, chrLabelTrack, drawLegend, drawVScale, featureInterval, groupToChrs, horPlace, horRulerTrack, mkIcon, renderTrackGlyphs, trackLegend, verPlace, zipMapsWith)
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId))
import Genetics.Browser.Types.Coordinates (BrowserPoint, CoordSys, Interval, Normalized(Normalized), lookupInterval)
import Genetics.Browser.View (Pixels)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Network.HTTP.Affjax as Affjax
import Unsafe.Coerce (unsafeCoerce)


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


gwasDraw :: forall a.
            Color
         -> a
         -> DrawingV
gwasDraw color =
  let r = 2.2
      c = circle 0.0 0.0 r
      out = outlined (outlineColor color) c
      fill = filled (fillColor color) c
  in const $ inj _point $ out <> fill



scoreVerPlace :: forall r1 r2.
                  { min :: Number, max :: Number | r1 }
               -> Feature (score :: Number | r2)
               -> Normalized Number
scoreVerPlace s =
  verPlace (to (\x -> Normalized $ (x.score - s.min) / (s.max - s.min)))


type GWASFeature r = Feature ( score :: Number
                             , chrId :: ChrId | r )



gemmaJSONParse :: forall a.
                  CoordSys ChrId a
               -> Json
               -> Maybe (GWASFeature ())
gemmaJSONParse cs j = do
  obj <- j ^? _Object
  chrId <- ChrId <$> obj ^? ix "chr" <<< _String
  pos   <- Bp    <$> obj ^? ix "ps"  <<< _Number
  score <-           obj ^? ix "af"  <<< _Number

  {chrSize} <- lookupInterval cs chrId

  pure { position: inj _point pos
       , frameSize: chrSize
       , score
       , chrId }



type GeneRow r = ( geneID :: String
                 , desc :: String
                 , start :: Bp
                 , end :: Bp
                 , name :: String
                 , chrId :: ChrId | r )

type Gene r = Feature (GeneRow r)



geneDraw :: forall r.
            (Gene r)
         -> DrawingV
geneDraw gene = inj _range \w ->
  let (Pair l r) = featureInterval gene
      glyphW = unwrap $ (r - l) / gene.frameSize
      rect = rectangle 0.0 0.0 (w * glyphW) 12.0
      out  = outlined (outlineColor aqua <> lineWidth 4.0) rect
      fill = filled   (fillColor teal) rect

  in out <> fill


geneRenderer :: forall r. Renderer (Gene r)
geneRenderer =
  { draw: geneDraw
  , horPlace
  , verPlace: const (Normalized 0.1) }


geneJSONParse :: CoordSys _ _
              -> Json
              -> Maybe (Gene ())
geneJSONParse cs j = do
  obj    <- j ^? _Object
  geneID <- obj ^? ix "Gene stable ID" <<< _String
  desc   <- obj ^? ix "Gene description" <<< _String
  name   <- obj ^? ix "Gene name" <<< _String
  start  <- Bp    <$> obj ^? ix "Gene start (bp)"  <<< _Number
  end    <- Bp    <$> obj ^? ix "Gene end (bp)"  <<< _Number
  chrId  <- ChrId <$> obj ^? ix "ChrId" <<< _String

  {chrSize} <- lookupInterval cs chrId

  pure { position: inj _range (Pair start end)
       , frameSize: chrSize
       , geneID, desc, name, start, end, chrId }


fetchGeneJSON :: CoordSys _ _ -> String -> Aff _ (Array (Gene ()))
fetchGeneJSON cs = fetchJSON (geneJSONParse cs)


type AnnotRow r = ( geneID :: String
                   , desc :: String
                   , name :: String
                   , chrId :: ChrId | r )
type Annot r = Feature (AnnotRow r)


fetchAnnotJSON :: CoordSys _ _
               -> String
               -> Aff _ (Array (Annot ()))
fetchAnnotJSON cs str = map toAnnot <$> fetchJSON (geneJSONParse cs) str
  where toAnnot :: Gene () -> Annot ()
        toAnnot gene = { geneID: gene.geneID
                       , desc: gene.desc
                       , position: gene.position
                       , frameSize: gene.frameSize
                       , name: gene.name
                       , chrId: gene.chrId }


getGWAS :: CoordSys ChrId _
        -> String
        -> Aff _ (Map ChrId (List (GWASFeature ())))
getGWAS cs url = map List.fromFoldable
                  <$> groupToChrs
                  <$> fetchJSON (gemmaJSONParse cs) url


getAnnotations :: CoordSys ChrId _
               -> String
               -> Aff _ (Map ChrId (List (Annot ())))
getAnnotations cs url = map List.fromFoldable
                  <$> groupToChrs
                  <$> fetchAnnotJSON cs url


getGenes :: CoordSys ChrId _
         -> String
         -> Aff _ (Map ChrId (List (Gene ())))
getGenes cs url = map List.fromFoldable
                  <$> groupToChrs
                  <$> fetchJSON (geneJSONParse cs) url




pointRenderer :: forall r1 r2.
                 { min :: Number, max :: Number, sig :: Number | r2 }
              -> (Feature (score :: Number | r1) -> DrawingV)
              -> Renderer (Feature (score :: Number | r1))
pointRenderer s draw = { horPlace, verPlace: scoreVerPlace s, draw }

basicRenderers :: forall r.
                  { min :: Number, max :: Number, sig :: Number | r }
               -> { gwas        :: Renderer (GWASFeature _)
                  , annotations :: Renderer (Annot _)
                  , genes       :: Renderer (Gene _)
                  }
basicRenderers s = { gwas:        pointRenderer s (gwasDraw navy)
                   , annotations: pointRenderer s annotDraw
                   , genes:       geneRenderer
                   }



-- TODO Configgable Annotation -> LegendEntry function (somehow?!)
annotLegendEntry :: forall r. Annot r -> LegendEntry
annotLegendEntry a =
  if (String.length a.name) `mod` 2 == 0
    then mkIcon blue "even length name"
    else mkIcon red  "odd length name"


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
                  (fillColor black) an.name




demoBatchBrowser :: forall f.
                    Traversable f
                 => CoordSys ChrId BrowserPoint
                 -> Canvas.Dimensions
                 -> Padding
                 -> { legend :: Legend, vscale :: VScale }
                 -> { gwas        :: Maybe (Map ChrId (f (GWASFeature ()         )))
                    , annotations :: Maybe (Map ChrId (f (Annot (score :: Number))))
                    , genes       :: Maybe (Map ChrId (f (Gene ()                ))) }
                 -> Interval BrowserPoint
                 -> { tracks :: Array Glyph
                    , relativeUI :: Drawing
                    , fixedUI :: Drawing }
demoBatchBrowser cs cdim padding ui input =
  let height = cdim.height - 2.0 * padding.vertical
      width  = cdim.width

      trackCanvas = { width: width - ui.vscale.width - ui.legend.width
                    , height }

      -- TODO make a type that corresponds to left-of-track and right-of-track to make this dynamic
      drawOverlay x w d =
        (translate x zero
         $ filled (fillColor white)
         $ rectangle zero zero w cdim.height )
        <> translate x padding.vertical
           d

      vScale = drawOverlay
                 zero
                 ui.vscale.width
                 (drawVScale ui.vscale height)

      legend = drawOverlay
                 (cdim.width - ui.legend.width)
                 ui.legend.width
                 (drawLegend ui.legend height)


      overlay = vScale <> legend


      renderer = basicRenderers ui.vscale

      gwas        = foldMap (map Array.fromFoldable) input.gwas
      annotations = foldMap (map Array.fromFoldable) input.annotations
      genes       = foldMap (map Array.fromFoldable) input.genes


      tracks :: Interval BrowserPoint -> Array Glyph
      tracks v =   renderTrackGlyphs cs renderer.gwas gwas trackCanvas v
                <> renderTrackGlyphs cs renderer.genes genes trackCanvas v
                <> renderTrackGlyphs cs renderer.annotations annotations trackCanvas v


      chrLabels = chrLabelTrack cs cdim
      ruler     = horRulerTrack ui.vscale red cdim
      boxes     = boxesTrack height cs cdim

      trackUI = chrLabels
              <> translate 0.0 padding.vertical <<< (ruler <> boxes)

  in \view -> { tracks: tracks view
              , relativeUI: trackUI view
              , fixedUI: overlay
              }




mouseChrIds :: Array ChrId
mouseChrIds =
            [ (ChrId "1")
            , (ChrId "2")
            , (ChrId "3")
            , (ChrId "4")
            , (ChrId "5")
            , (ChrId "6")
            , (ChrId "7")
            , (ChrId "8")
            , (ChrId "9")
            , (ChrId "10")
            , (ChrId "11")
            , (ChrId "12")
            , (ChrId "13")
            , (ChrId "14")
            , (ChrId "15")
            , (ChrId "16")
            , (ChrId "17")
            , (ChrId "18")
            , (ChrId "19")
            , (ChrId "X")
            , (ChrId "Y")
            ]


mouseChrs :: Map ChrId Bp
mouseChrs = Map.fromFoldable $ Array.zip mouseChrIds $
            [ (Bp 195471971.0)
            , (Bp 182113224.0)
            , (Bp 160039680.0)
            , (Bp 156508116.0)
            , (Bp 151834684.0)
            , (Bp 149736546.0)
            , (Bp 145441459.0)
            , (Bp 129401213.0)
            , (Bp 124595110.0)
            , (Bp 130694993.0)
            , (Bp 122082543.0)
            , (Bp 120129022.0)
            , (Bp 120421639.0)
            , (Bp 124902244.0)
            , (Bp 104043685.0)
            , (Bp 98207768.0)
            , (Bp 94987271.0)
            , (Bp 90702639.0)
            , (Bp 61431566.0)
            , (Bp 17103129.0)
            , (Bp 9174469.0)
            ]

mouseColors :: ChrCtx (color :: Color)
mouseColors = Map.fromFoldable $ Array.zip mouseChrIds $ map (\x -> {color: x})
              [ navy, blue, aqua, teal, olive
              , green, lime, yellow, orange, red
              , maroon, fuchsia, purple, navy, blue
              , aqua, teal, olive, green, lime
              , yellow ]


mouseChrCtx :: ChrCtx (size :: Bp)
mouseChrCtx = map (\size -> { size }) mouseChrs
