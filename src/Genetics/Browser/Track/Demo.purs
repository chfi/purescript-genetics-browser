module Genetics.Browser.Track.Demo
       ( demoBrowser
       , demoLegend
       , getDataDemo
       ) where

import Prelude

import Color (Color, black, white)
import Color.Scheme.Clrs (aqua, blue, fuchsia, green, lime, maroon, navy, olive, orange, purple, red, teal, yellow)
import Control.Monad.Aff (Aff)
import Data.Argonaut (Json, _Number, _Object, _String)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Lens (to, (^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Data.Pair (Pair(..))
import Data.Symbol (SProxy(SProxy))
import Data.Traversable (class Traversable)
import Data.Variant (inj)
import Genetics.Browser.Track.Backend (BrowserTrack, ChrCtx, DrawingV, Feature, PureRenderer, _point, _range, bumpFeatures, chrLabelTrack, drawLegend, drawVScale, groupToChrs, horPlace, horRulerTrack, mkIcon, renderTrack, verPlace, zipMapsWith)
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId))
import Genetics.Browser.Types.Coordinates (BrowserPoint, CoordSys, Interval, Normalized(Normalized), lookupInterval)
import Genetics.Browser.View (Pixels)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)


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


annotDraw :: forall r.
             (Feature (name :: String | r))
          -> DrawingV
annotDraw an = inj _point $ out <> fill <> text'
  where rad = 5.0
        c = circle zero zero rad
        out  = outlined (outlineColor maroon <> lineWidth 3.0) c
        fill = filled   (fillColor red) c
        text' = Drawing.text
                  (font sansSerif 12 mempty)
                  (7.5) (2.5)
                  (fillColor black) an.name


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
       , geneID
       , desc
       , name
       , start
       , end
       , chrId
       }


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


getDataDemo :: CoordSys _ _
            -> { gwas :: String
               , annots :: String }
            -> Aff _ { gwas   :: Map ChrId (List (GWASFeature ()       ))
                     , annots :: Map ChrId (List (Annot (score :: Number)))
                     }
getDataDemo cs urls = do
  gwas    <- fetchJSON (gemmaJSONParse cs) urls.gwas
  annots' <- fetchAnnotJSON cs urls.annots
  -- Divide data by chromosomes
  let gwasChr :: Map ChrId (List (GWASFeature ()))
      gwasChr = List.fromFoldable <$> groupToChrs gwas
  let annotsChr :: Map ChrId (List (Annot ()))
      annotsChr = List.fromFoldable <$> groupToChrs annots'
  let bumpedAnnots :: Map ChrId (List (Annot (score :: Number)))
      bumpedAnnots = zipMapsWith
                     (bumpFeatures (to (_.score)) (SProxy :: SProxy "score")
                       (Bp 1000000.0))
                     gwasChr annotsChr

  pure { gwas: gwasChr, annots: bumpedAnnots }


demoTrack :: forall f r.
             Foldable f
          => Traversable f
          => CoordSys ChrId BrowserPoint
          -> { min :: Number, max :: Number, sig :: Number }
          -> { gwas   :: Map ChrId (f (GWASFeature ()))
             , annots :: Map ChrId (f (Annot (score :: Number))) }
          -> BrowserTrack
demoTrack cs s {gwas, annots} =
  let renderers = renderersDemo s

      gwasTrack   = renderTrack cs renderers.gwas gwas
      annotsTrack = renderTrack cs renderers.annots annots

      chrLabels = chrLabelTrack cs
      ruler = horRulerTrack s red

  in chrLabels
  <> ruler
  <> gwasTrack
  <> annotsTrack



pointRenderer :: forall r.
                 { min :: Number, max :: Number, sig :: Number }
              -> (Feature (score :: Number | r) -> DrawingV)
              -> PureRenderer (Feature (score :: Number | r))
pointRenderer s draw = { horPlace, verPlace: scoreVerPlace s, draw }


renderersDemo :: forall r.
                 { min :: Number, max :: Number, sig :: Number }
              -> { gwas   :: PureRenderer _
                 , annots :: PureRenderer _
                 }
renderersDemo s = { gwas:   pointRenderer s (gwasDraw navy)
                  , annots: pointRenderer s annotDraw }


demoLegend :: Array {text :: String, icon :: Drawing}
demoLegend =
  [ mkIcon red "Red thing"
  , mkIcon blue "blue thing"
  , mkIcon purple "boop"
  ]

              -- the first 2 are used by all parts of the browser
demoBrowser :: forall f.
               Traversable f
            => CoordSys ChrId BrowserPoint
            -> Canvas.Dimensions
            -> Pixels
            -- this by the vertical scale and the tracks
            -> { min :: Number, max :: Number, sig :: Number }
            -- these define the width of the non-track parts of the browser;
            -- the browser takes up the remaining space.
            -> { vScaleWidth :: Pixels
               , legendWidth :: Pixels }
            -- vScale only
            -> Color
            -- to be drawn in the legend on the RHS
            -> Array { text :: String, icon :: Drawing }
            -- finally, used by the main track
            -> { gwas   :: Map ChrId (f (GWASFeature ()))
               , annots :: Map ChrId (f (Annot (score :: Number))) }
            -> Interval BrowserPoint
            -- the drawing produced contains all 3 parts.
            -> { track :: Drawing, overlay :: Drawing }
demoBrowser cs canvas vpadding vscale sizes vscaleColor legend {gwas, annots} =
  let height = canvas.height - 2.0 * vpadding
      width  = canvas.width

      trackCanvas = { width: canvas.width - sizes.vScaleWidth - sizes.legendWidth
                    , height }

      bg w' h' = filled (fillColor white) $ rectangle 0.0 0.0 w' h'

      -- TODO unify vertical offset by padding further; do it as late as possible

      vScale _ = let w = sizes.vScaleWidth
                 in  translate zero vpadding
                   $ bg w height
                  <> drawVScale w height vscale vscaleColor

      track v = demoTrack cs vscale {gwas, annots} trackCanvas v

      legendD _ = let w = sizes.legendWidth
                  in translate (canvas.width - w) vpadding
                    $ bg w height
                   <> drawLegend sizes.legendWidth canvas.height legend

  in \view -> { track: track view
              , overlay: vScale view <> legendD view }




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
