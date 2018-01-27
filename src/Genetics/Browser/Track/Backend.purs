module Genetics.Browser.Track.Backend
       ( demoBrowser
       , demoLegend
       , getDataDemo
       ) where

import Prelude

import Color (Color, black, white)
import Color.Scheme.Clrs (aqua, blue, fuchsia, green, lime, maroon, navy, olive, orange, purple, red, teal, yellow)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (class Foldable, fold, foldMap, foldl, length, maximum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Lens (Getter', to, view, (^.), (^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Pair (Pair(..))
import Data.Record as Record
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(Tuple), snd, uncurry)
import Data.Variant (Variant, case_, inj, on, onMatch)
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId))
import Genetics.Browser.Types.Coordinates (BrowserPoint, CoordSys, Interval, Normalized(Normalized), _BrowserIntervals, _Index, _Interval, intervalToScreen, intervalsOverlap, intervalsToMap, lookupInterval)
import Genetics.Browser.View (Pixels)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Network.HTTP.Affjax as Affjax
import Type.Prelude (class RowLacks)


type BrowserView = Interval BrowserPoint


intersection :: forall k a b.
                Ord k
             => Map k a
             -> Map k b
             -> Map k a
intersection a b = Map.filterKeys (flip Map.member b) a

zipMaps :: forall k a b.
           Ord k
        => Map k a
        -> Map k b
        -> Map k (Tuple a b)
zipMaps a b =
  let kas = Array.unzip $ Map.toAscUnfoldable $ a `intersection` b
      kbs = Array.unzip $ Map.toAscUnfoldable $ b `intersection` a
      kabs = uncurry Array.zip $ map (Array.zip (snd kas)) kbs
  in Map.fromFoldable kabs


zipMapsWith :: forall k a b c.
               Ord k
            => (a -> b -> c)
            -> Map k a
            -> Map k b
            -> Map k c
zipMapsWith f a b = uncurry f <$> zipMaps a b


type ChrCtx r = Map ChrId (Record r)


_point = SProxy :: SProxy "point"
_range = SProxy :: SProxy "range"

type GenomePosV = Variant ( point :: Bp
                          , range :: Interval Bp )

type FeatureR r = ( position :: GenomePosV
                  , frameSize :: Bp
                  | r )

type Feature r = Record (FeatureR r)


type DrawingR  = ( point :: Drawing
                 , range :: Pixels -> Drawing )

type DrawingV  = Variant DrawingR

type HorPlaceR = ( point :: Normalized Number
                 , range :: Interval (Normalized Number) )

type HPos = Variant HorPlaceR



type PureRenderer a = { draw     :: a -> DrawingV
                      , horPlace :: a -> HPos
                      , verPlace :: a -> Normalized Number
                      }


type ReadyGlyph = { drawing :: DrawingV
                  , horPos  :: HPos
                  , verPos  :: Normalized Number
                  }


horPlace :: forall r.
            Feature r
         -> HPos
horPlace {position, frameSize} =
  let f p = Normalized (unwrap $ p / frameSize)
  in case_
    # onMatch
      { point: inj _point <<< f
      , range: inj _range <<< (map f)
      }
    $ position


verPlace :: forall r.
            (Getter' (Feature r) (Normalized Number))
         -> Feature r
         -> Normalized Number
verPlace f ft = ft ^. f



render :: forall err a.
          PureRenderer a
       -> a
       -> ReadyGlyph
render render a =
      { drawing: render.draw a
      , horPos:  render.horPlace a
      , verPos:  render.verPlace a
      }



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





type CanvasReadyDrawing = Drawing

type BrowserTrack = Canvas.Dimensions
                 -> Interval BrowserPoint
                 -> CanvasReadyDrawing


-- Given a frame-normalized drawing, finalize it so it can be rendered to a canvas;
-- `width` is the width in pixels of the frame the drawing resides in.
resizeDrawing :: forall r.
                 { width :: Pixels | r }
              -> DrawingV
              -> Drawing
resizeDrawing {width} = case_
  # onMatch
     { point: (\x -> x)
     , range: (_ $ width) }

drawIntervalFeature :: forall f r a.
                     Foldable f
                  => Canvas.Dimensions
                  -> { width :: Pixels, offset :: Pixels }
                  -> f ReadyGlyph
                  -> CanvasReadyDrawing
drawIntervalFeature {height} {width, offset} = foldMap drawIt
  where drawIt {drawing, horPos, verPos} =
          let x' = case_
                    # on _point (\x -> x)
                    # on _range (\(Pair l _) -> l)
                    $ horPos
              x = width * unwrap x'
              y = height * (1.0 - unwrap verPos)
              d = resizeDrawing {width} drawing
          in translate (x + offset) y d




-- | Returns the left-hand-edge offset, and width, in pixels,
-- | of each interval (chromosome) in the provided coordinate system
viewportIntervals :: forall i a r.
                     Ord i
                  => CoordSys i BrowserPoint
                  -> Canvas.Dimensions
                  -> Interval BrowserPoint
                  -> Map i { width :: Pixels, offset :: Pixels }
viewportIntervals cs cdim bView =
  intervalToScreen cdim bView <$> (view _Interval <$> intervalsToMap cs)



pureTrack :: forall f a.
             Foldable f
          => CoordSys ChrId BrowserPoint
          -> Map ChrId (f ReadyGlyph)
          -> BrowserTrack
pureTrack cs drawings cd v =
  fold $ zipMapsWith (drawIntervalFeature cd) (viewportIntervals cs cd v) drawings


renderTrack :: forall f a.
                 Foldable f
              => Traversable f
              => CoordSys ChrId BrowserPoint
              -> PureRenderer a
              -> Map ChrId (f a)
              -> BrowserTrack
renderTrack cs r chrs =
  let validDrawings :: Map ChrId (f ReadyGlyph)
      validDrawings = (map <<< map) (render r) chrs
  in pureTrack cs validDrawings




horRulerTrack :: forall r r1.
                 { min :: Number, max :: Number, sig :: Number }
              -> Color
              -> BrowserTrack
horRulerTrack {min, max, sig} color f _ = outlined outline rulerDrawing
  where normY = (sig - min) / (max - min)
        y = f.height - (normY * f.height)
        outline = outlineColor color
        rulerDrawing = Drawing.path [{x: 0.0, y}, {x: f.width, y}]


chrLabelTrack :: CoordSys ChrId BrowserPoint
              -> BrowserTrack
chrLabelTrack cs = pureTrack cs (Map.fromFoldable results)
  where mkLabel :: ChrId -> ReadyGlyph
        mkLabel chr = { drawing: inj _point $ chrText chr
                      , horPos:  inj _point $ Normalized $ one
                      , verPos: Normalized (-0.1) }

        font' = font sansSerif 12 mempty

        chrText :: ChrId -> Drawing
        chrText chr = Drawing.text font' zero zero (fillColor black) (unwrap chr)

        results :: Array _
        results = map (\x -> Tuple (x^._Index) [mkLabel (x^._Index)]) $ cs^._BrowserIntervals



drawVScale :: forall r.
              Number
           -> Number
           -> { min :: Number, max :: Number, sig :: Number }
           -> Color
           -> Drawing
drawVScale width height vs col =
  -- should have some padding here too; hardcode for now
  let hPad = width  / 5.0
      vPad = height / 10.0
      x = width / 2.0

      y1 = 0.0
      y2 = height

      n = (_ * 0.1) <<< Int.toNumber <$> (0 .. 10)

      p = Drawing.path [{x, y:y1}, {x, y:y2}]

      bar w y = Drawing.path [{x:x-w, y}, {x, y}]

      ps = foldMap (\i -> bar
                          (if i == 0.0 || i == 1.0 then 8.0 else 3.0)
                          (y1 + i*(y2-y1))) n

      ft = font sansSerif 10 mempty
      mkT y = Drawing.text ft (x-12.0) y (fillColor black)

      topLabel = mkT (y1-(0.5*vPad)) $ show vs.max
      btmLabel = mkT (y2+vPad) $ show vs.min

  in outlined (outlineColor col <> lineWidth 2.0) (p <> ps)
  <> topLabel <> btmLabel



drawLegendItem :: Number
               -> { text :: String, icon :: Drawing }
               -> Drawing
drawLegendItem w {text, icon} =
  let ft = font sansSerif 12 mempty
      t = Drawing.text ft 12.0 0.0 (fillColor black) text
  in icon <> t


drawLegend :: Number
           -> Number
           -> Array { text :: String, icon :: Drawing }
           -> Drawing
drawLegend width height icons =
  let hPad = width  / 5.0
      vPad = height / 5.0
      n :: Int
      n = length icons
      x = hPad
      f :: Number -> { text :: String, icon :: Drawing } -> Drawing
      f y ic = translate x y $ drawLegendItem (width - 2.0*hPad) ic
      d = (height - 2.0*vPad) / (length icons)
      ds = mapWithIndex (\i ic -> f (vPad+(vPad*(Int.toNumber i))) ic) icons
  in fold ds


mkIcon :: Color -> String -> { text :: String, icon :: Drawing }
mkIcon c text =
  let sh = circle (-2.5) (-2.5) 5.0
      icon = outlined (outlineColor c <> lineWidth 2.0) sh <>
             filled (fillColor c) sh
  in {text, icon}





featureInterval :: forall a. Feature a -> Interval Bp
featureInterval {position} = case_
  # onMatch
     { point: (\x -> Pair x x)
     , range: (\x -> x)
     }
  $ position


bumpFeatures :: forall f a l i o.
                Foldable f
             => Functor f
             => RowCons l Number (FeatureR i) (FeatureR o)
             => RowLacks l (FeatureR i)
             => IsSymbol l
             => Getter' (Feature a) Number
             -> SProxy l
             -> Bp
             -> f (Feature a)
             -> f (Feature i)
             -> f (Feature o)
bumpFeatures f l radius other = map bumpAnnot
  where maxInRadius :: Interval Bp -> Number
        maxInRadius lr = fromMaybe 0.0 $ maximum
                          $ map (\g -> if intervalsOverlap (featureInterval g) lr
                                          then f `view` g else 0.0) other

        bumpAnnot :: Record (FeatureR i) -> Record (FeatureR o)
        bumpAnnot a =
          let y = maxInRadius (featureInterval a)
          in Record.insert l y a



groupToChrs :: forall a f rData.
               Monoid (f {chrId :: ChrId | rData})
            => Foldable f
            => Applicative f
            => f { chrId :: ChrId | rData }
            -> Map ChrId (f { chrId :: ChrId | rData })
groupToChrs = foldl (\chrs r@{chrId} -> Map.alter (add r) chrId chrs ) mempty
  where add x Nothing   = Just $ pure x
        add x (Just xs) = Just $ pure x <> xs

-- MOVE TO DEMO.PURS

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
scoreVerPlace' :: forall r1 r2.
                  { min :: Number, max :: Number | r1 }
               -> Feature (score :: Number | r2)
               -> Normalized Number
scoreVerPlace' s =
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


fetchGemmaJSON :: CoordSys _ _ -> String -> Aff _ (Array (GWASFeature ()))
fetchGemmaJSON cs = fetchJSON (gemmaJSONParse cs)


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
  gwas    <- fetchGemmaJSON cs urls.gwas
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


renderersDemo :: forall r.
                 { min :: Number, max :: Number, sig :: Number }
              -> { gwas   :: PureRenderer _
                 , annots :: PureRenderer _
                 }
renderersDemo s = { gwas, annots }
  where gwas = { draw:  gwasDraw navy
               , horPlace
               , verPlace: scoreVerPlace' s }

        annots = { draw: annotDraw
                 , horPlace
                 , verPlace: scoreVerPlace' s }


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
            -> BrowserView
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
