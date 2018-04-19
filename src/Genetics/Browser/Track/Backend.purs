module Genetics.Browser.Track.Backend where

import Prelude

import Color (Color, black, white)
import Color.Scheme.Clrs (red)
import Data.Array ((..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Foldable (class Foldable, fold, foldMap, foldl, length, maximum, minimum)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Lens (Getter', view, (^.))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (unwrap)
import Data.Number.Format as Num
import Data.Pair (Pair(..))
import Data.Record as Record
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Variant (Variant, case_, inj, onMatch)
import Genetics.Browser.Types (Bp, ChrId)
import Genetics.Browser.Types.Coordinates (CoordSys, CoordSysView(..), Normalized(Normalized), _Segments, aroundPair, pairSize, pairsOverlap, scaledSegments, scaledSegments', viewScale)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, FillStyle, OutlineStyle, Point, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Math (pow)
import Math as Math
import Type.Prelude (class RowLacks)


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


_point = SProxy :: SProxy "point"
_range = SProxy :: SProxy "range"

_batch = (SProxy :: SProxy "batch")
_single = (SProxy :: SProxy "single")

type Feature a = { position  :: Pair Bp
                 , frameSize :: Bp
                 , feature   :: a }


-- The very basic shapes a glyph can consist of (for now)
data GlyphShape
  = GCircle Number
  | GRect Number Number
  | GMany (List GlyphShape)

-- A `GlyphDrawing` is a shape plus styles informing how to render it (from purescript-drawing)
data GlyphDrawing
  = GDrawing OutlineStyle FillStyle GlyphShape


type NPoint = { x :: Normalized Number
              , y :: Normalized Number }

type DrawingR  = ( point :: Drawing
                 , range :: Number
                         -> { drawing :: Unit -> Drawing
                            , width :: Number } )
type DrawingV  = Variant DrawingR

type HorPlaceR = ( point :: Normalized Number
                 , range :: Pair (Normalized Number) )
type HPos = Variant HorPlaceR


type BatchRenderer a = { drawing :: Drawing
                       , place :: a -> Normalized Point }

type SingleRenderer a = { draw  :: a -> DrawingV
                        , horPlace :: a -> HPos
                        , verPlace :: a -> Normalized Number }

type OldRenderer a = Variant ( batch :: BatchRenderer a
                             , single :: SingleRenderer a )


type NormalizedGlyph = { drawing :: Variant DrawingR
                       , horPos  :: Variant HorPlaceR
                       , verPos  :: Normalized Number
                       }

type SingleGlyph = { drawing :: Unit -> Drawing, point :: Point, width :: Number }



horRulerTrack :: forall r.
                 { min :: Number, max :: Number, sig :: Number | r }
              -> Color
              -> Canvas.Dimensions
              -> Drawing
horRulerTrack {min, max, sig} color f = outlined outline rulerDrawing <> label
  where normY = (sig - min) / (max - min)
        thickness = 2.0
        outline = outlineColor color <> lineWidth thickness
        y = thickness + f.height - (normY * f.height)
        rulerDrawing = Drawing.path [{x: 0.0, y}, {x: f.width, y}]

        expSig = 10.0 `pow` (-sig)
        text  = "P = " <> (Num.toStringWith (Num.exponential 1) expSig)

        font' = font sansSerif 16 mempty
        label = Drawing.text font' (f.width+4.0) (y-6.0) (fillColor red) text


chrLabelTrack :: CoordSys ChrId BigInt
              -> Canvas.Dimensions
              -> Map ChrId (Array NormalizedGlyph)
chrLabelTrack cs cdim =
  let font' = font sansSerif 12 mempty

      chrText :: ChrId -> Drawing
      chrText chr =
        Drawing.text font' zero zero (fillColor black) (unwrap chr)

      mkLabel :: ChrId -> NormalizedGlyph
      mkLabel chr = { drawing: inj _point $ chrText chr
                    , horPos:  inj _point $ Normalized (0.5)
                    , verPos: Normalized (0.03) }

  in mapWithIndex (\i _ -> [mkLabel i]) $ cs ^. _Segments


chrLabelTrack' :: UISlot
               -> Map ChrId (Pair Number)
               -> Tuple UISlot (Array DrawingN)
chrLabelTrack' slot segs =
  let font' = font sansSerif 12 mempty

      chrText :: ChrId -> Drawing
      chrText chr =
        Drawing.text font' zero zero (fillColor black) (unwrap chr)


      y = slot.offset.y - (0.5 * slot.size.height)

      label :: ChrId -> Pair Number -> Array DrawingN
      label chr seg@(Pair l _) =
        let drawing = chrText chr
            point = { x: slot.offset.x + l + (pairSize seg) * 0.5, y }
        in [{ drawing, points: [point] }]

  in Tuple slot (foldMapWithIndex label segs)



-- boxesTrack :: Number
--            -> CoordSys ChrId BigInt
--            -> Canvas.Dimensions
--            -> Pair BigInt -> CanvasReadyDrawing
-- boxesTrack h cs = unsafeCoerce unit
-- boxesTrack h cs = drawRelativeUI cs $ map (const [glyph]) $ cs ^. _BrowserIntervals
--   where glyph :: NormalizedGlyph
--         glyph = { drawing: inj _range draw
--                 , horPos:  inj _range $ Normalized <$> (Pair zero one)
--                 , verPos: aone }
--         draw = \w ->
--              let rect = rectangle 0.0 0.0 w h
--              in outlined (outlineColor black <> lineWidth 1.5) rect



bumpFeatures :: forall f a l i o.
                Foldable f
             => Functor f
             => RowCons l Number i o
             => RowLacks l i
             => IsSymbol l
             => Getter' (Feature (Record a)) Number
             -> SProxy l
             -> Bp
             -> f (Feature (Record a))
             -> f (Feature (Record i))
             -> f (Feature (Record o))
bumpFeatures f l radius other = map bump
  where minInRadius :: Pair Bp -> Number
        minInRadius lr = fromMaybe 1.0
                         $ minimum
                          $ map (\g -> if pairsOverlap g.position lr
                                          then f `view` g else 1.0) other

        bump :: Feature (Record i) -> Feature (Record o)
        bump a =
          let y = minInRadius (aroundPair radius a.position)
          in { position: a.position
             , frameSize: a.frameSize
             , feature: Record.insert l y a.feature }


type VScaleRow r = ( min :: Number
                   , max :: Number
                   , sig :: Number
                   | r )

type VScale = { width :: Number
              , color :: Color
              | VScaleRow () }


negLog10 :: Number -> Number
negLog10 p = - ((Math.log p) / Math.ln10)

drawVScale :: forall r.
              VScale
           -> Number
           -> Drawing
drawVScale vscale height =
  let
      hPad = vscale.width  / 8.0
      x = 7.0 * hPad

      numSteps = 3
      spokes = (_ / (Int.toNumber numSteps))
               <<< Int.toNumber <$> (0 .. numSteps)

      barOutline = outlineColor vscale.color <> lineWidth 2.0

      vBar = Drawing.path [{x, y: 0.0}, {x, y: height}]

      hBar w y = Drawing.path [{x:x-w, y}, {x, y}]
      bars = vBar
          <> foldMap (\i -> hBar 8.0 (i * height)) spokes


      unitLabel =
        Drawing.translate (vscale.width * 0.5 - hPad) (height * 0.72)
        $ Drawing.rotate (- Math.pi / 2.0)
        $ Drawing.text (font sansSerif 18 mempty)
            0.0 0.0 (fillColor black) "-log10 (P value)"

      label yN = Drawing.text
                    (font sansSerif 14 mempty)
                    (vscale.width * 0.6 - hPad)
                    (yN * height + 5.0)
                    (fillColor black)
                 $ Num.toStringWith (Num.fixed 0)
                 $ (\p -> min vscale.max p)
                 $ vscale.min + (1.0 - yN) * (vscale.max - vscale.min)

      labels = foldMap label spokes

  in outlined barOutline bars <> labels <> unitLabel


type LegendEntry = { text :: String, icon :: Drawing }

type Legend = { width :: Number
              , entries :: Array LegendEntry }



mkIcon :: Color -> String -> LegendEntry
mkIcon c text =
  let sh = circle 0.0 0.0 5.0
      icon = outlined (outlineColor black <> lineWidth 2.0) sh <>
             filled (fillColor c) sh
  in {text, icon}


drawLegendItem :: LegendEntry
               -> Drawing
drawLegendItem {text, icon} =
  let ft = font sansSerif 12 mempty
      t = Drawing.text ft 12.0 0.0 (fillColor black) text
  in icon <> t


drawLegend :: Legend
           -> Number
           -> Drawing
drawLegend {width, entries} height =
  let hPad = width  / 5.0
      vPad = height / 5.0
      n :: Int
      n = length entries
      x = hPad
      f :: Number -> { text :: String, icon :: Drawing } -> Drawing
      f y ic = translate x y $ drawLegendItem ic
      d = (height - 2.0*vPad) / (length entries)
      ds = mapWithIndex (\i ic -> f (vPad+(vPad*(Int.toNumber i))) ic) entries
  in fold ds


type Padding = { vertical :: Number
               , horizontal :: Number
               }


groupToMap :: forall i a f rData.
              Monoid (f a)
           => Ord i
           => Foldable f
           => Applicative f
           => (a -> i)
           -> f a
           -> Map i (f a)
groupToMap f = foldl (\grp a -> Map.alter (add a) (f a) grp ) mempty
  where add :: a -> Maybe (f a) -> Maybe (f a)
        add x xs = (pure $ pure x) <> xs



eqLegend a b = a.text == b.text

trackLegend :: forall f a.
               Foldable f
            => Functor f
            => (a -> LegendEntry)
            -> f a
            -> Array LegendEntry
trackLegend f as = Array.nubBy eqLegend $ Array.fromFoldable $ map f as



horPlaceOnSegment :: forall r.
                     Pair Number
                  -> { horPos :: Variant HorPlaceR | r }
                  -> Number
horPlaceOnSegment segmentPixels o =
    case_
  # onMatch { point: \(Normalized x) -> x * pairSize segmentPixels
            , range: \(Pair l r)     -> unwrap l * pairSize segmentPixels }
  $ o.horPos


finalizeNormDrawing :: forall r.
                       Pair Number
                    -> { drawing :: DrawingV | r }
                    -> { drawing :: Unit -> Drawing, width :: Number }
finalizeNormDrawing seg o =
    case_
  # onMatch { point: \x -> { drawing: \_ -> x, width: 1.0 }
            , range: (_ $ pairSize seg) }
  $ o.drawing



renderNormalized1 :: Number
                  -> Pair Number
                  -> NormalizedGlyph
                  -> SingleGlyph
renderNormalized1 height seg@(Pair l _) ng =
  let x = horPlaceOnSegment seg ng + l
      y = height * (one - unwrap ng.verPos)
      {drawing, width} = finalizeNormDrawing seg ng
  in { point: {x,y}, drawing, width }


rescaleNormSingleGlyphs :: Number
                        -> Pair Number
                        -> Array NormalizedGlyph
                        -> Array SingleGlyph
rescaleNormSingleGlyphs height seg =
  map (renderNormalized1 height seg)


withPixelSegments :: forall r m.
                     Monoid m
                  => CoordSys ChrId BigInt
                  -> { width :: Number | r }
                  -> Pair BigInt
                  -> (ChrId -> Pair Number -> m)
                  -> m
withPixelSegments cs cdim bView =
  let scale = { screenWidth: cdim.width
              , viewWidth: pairSize bView }
  in flip foldMapWithIndex (scaledSegments cs scale)



type UISlot = { offset :: Point
              , size   :: Canvas.Dimensions }

type UISlots = { left   :: UISlot
               , right  :: UISlot
               , top    :: UISlot
               , bottom :: UISlot }

--------------------------


type DrawingN = { drawing :: Drawing, points :: Array Point }
type Label = { text :: String, point :: Point }




type RenderedTrack a = { features :: Array a
                       , drawings :: Array DrawingN
                       , labels   :: Array Label
                       , overlaps :: Number -> Point -> Array a }


type Renderer a =
     Canvas.Dimensions
  -> Map ChrId (Array a)
  -> Map ChrId (Pair Number)
  -> RenderedTrack a




browser :: forall a b c.
           CoordSys ChrId BigInt
        -> Canvas.Dimensions
        -> Canvas.Dimensions
        -> UISlots
        -> { legend :: Legend, vscale :: VScale }
        -> { gwas        :: Renderer a
           , annotations :: Renderer b }
        -> { gwas        :: Map ChrId (Array a)
           , annotations :: Map ChrId (Array b) }
        -> { tracks     :: CoordSysView
                        -> { gwas :: RenderedTrack a
                           , annotations :: RenderedTrack b }
           , relativeUI :: CoordSysView -> Drawing
           , fixedUI    :: Drawing }
browser cs trackDim overlayDim uiSlots ui renderers inputTracks =
  let
      drawInSlot {offset, size} d =
          (translate offset.x offset.y
           $ filled (fillColor white)
           $ rectangle zero zero size.width size.height)
        <> translate offset.x offset.y d

      vScale = drawInSlot uiSlots.left
                 $ drawVScale ui.vscale uiSlots.left.size.height
      legend = drawInSlot uiSlots.right
                 $ drawLegend ui.legend uiSlots.right.size.height

      ruler   = Drawing.translate ui.vscale.width uiSlots.top.size.height
                $ horRulerTrack ui.vscale red trackDim

      fixedUI = vScale <> legend <> ruler


      segmentPadding = 12.0

      segmentPixels :: CoordSysView -> Map ChrId (Pair Number)
      segmentPixels vw =
        aroundPair (-segmentPadding)
        <$> scaledSegments' cs (viewScale trackDim vw)

      tracks :: CoordSysView -> _
      tracks =
        let gwasT  = renderers.gwas trackDim inputTracks.gwas
            annotT = renderers.annotations trackDim inputTracks.annotations
        in \v -> let segs = segmentPixels v
                 in { gwas: gwasT segs
                    , annotations: annotT segs }


      renderUIElement :: Map ChrId (Array NormalizedGlyph)
                      -> ChrId -> Pair Number -> Array SingleGlyph
      renderUIElement m k s
          = fold $ rescaleNormSingleGlyphs overlayDim.height s
                <$> (Map.lookup k m)

      drawTrackUI :: Pair BigInt -> (ChrId -> Pair Number -> (Array _)) -> Drawing
      drawTrackUI v = foldMap f <<< withPixelSegments cs trackDim v
        where f {drawing, point} = Drawing.translate point.x point.y (drawing unit)

      chrLabels :: _
      chrLabels = renderUIElement $ chrLabelTrack cs trackDim

      relativeUI :: CoordSysView -> Drawing
      relativeUI v = drawTrackUI (unwrap v) chrLabels

  in { tracks
     , relativeUI
     , fixedUI
     }
