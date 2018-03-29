module Genetics.Browser.Track.Backend where

import Prelude

import Color (Color, black, white)
import Color.Scheme.Clrs (navy, red)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, _Array)
import Data.Array ((..))
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Data.Exists (Exists, runExists)
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, fold, foldMap, foldl, length, maximum)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Lens (Getter', view, (^?), (^.))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (unwrap)
import Data.Pair (Pair(..))
import Data.Pair as Pair
import Data.Profunctor.Strong (fanout)
import Data.Record as Record
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Variant (Variant, case_, inj, onMatch)
import Genetics.Browser.Types (Bp, ChrId)
import Genetics.Browser.Types.Coordinates (CoordSys, CoordSysView(..), Normalized(..), ViewScale(..), _Segments, pairSize, pairsOverlap, scaledSegments, viewScale)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, FillStyle, OutlineStyle, Point, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Math as Math
import Network.HTTP.Affjax as Affjax
import Type.Prelude (class RowLacks)
import Unsafe.Coerce (unsafeCoerce)


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


-- Simplify & let all feature positions be represented as ranges
type GenPos = Pair Bp

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


type GenomePosV = Variant ( point :: Bp
                          , range :: Pair Bp )

type FeatureR r = ( position :: GenomePosV
                  , frameSize :: Bp
                  | r )
type Feature r = Record (FeatureR r)

type DrawingR  = ( point :: Drawing
                 , range :: Number
                         -> { drawing :: Unit -> Drawing
                            , width :: Number } )
type DrawingV  = Variant DrawingR

type HorPlaceR = ( point :: Normalized Number
                 , range :: Pair (Normalized Number) )
type HPos = Variant HorPlaceR

type HPosN = Pair (Normalized Number)

type BatchRenderer a = { drawing :: Drawing
                       , place :: a -> Normalized Point }

type SingleRenderer a = { draw  :: a -> DrawingV
                        , horPlace :: a -> HPos
                        , verPlace :: a -> Normalized Number }

type Renderer a = Variant ( batch :: BatchRenderer a
                          , single :: SingleRenderer a )


type NormalizedGlyph = { drawing :: Variant DrawingR
                       , horPos  :: Variant HorPlaceR
                       , verPos  :: Normalized Number
                       }


type BatchGlyph c = { drawing :: Drawing, points :: Array c }
type SingleGlyph = { drawing :: Unit -> Drawing, point :: Point, width :: Number }



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


renderSingle :: forall a.
                SingleRenderer a
             -> a
             -> NormalizedGlyph
renderSingle render a =
      { drawing: render.draw a
      , horPos:  render.horPlace a
      , verPos:  render.verPlace a
      }



renderBatch :: forall a.
               BatchRenderer a
            -> Array a
            -> BatchGlyph (Normalized Point)
renderBatch render as = {drawing, points}
  where drawing = render.drawing
        points  = map render.place as



render :: forall a.
          Renderer a
       -> Array a
       -> Either
           (BatchGlyph (Normalized Point))
           (Array NormalizedGlyph)
render r as = case_
  # onMatch { batch:  \r -> Left  $ renderBatch r as
            , single: \r -> Right $ map (renderSingle r) as }
  $ r



horRulerTrack :: forall r.
                 { min :: Number, max :: Number, sig :: Number | r }
              -> Color
              -> Canvas.Dimensions
              -> Drawing
horRulerTrack {min, max, sig} color f = outlined outline rulerDrawing
  where normY = (sig - min) / (max - min)
        y = f.height - (normY * f.height)
        outline = outlineColor color
        rulerDrawing = Drawing.path [{x: 0.0, y}, {x: f.width, y}]


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
                    , verPos: Normalized (0.05) }

  in mapWithIndex (\i _ -> [mkLabel i]) $ cs ^. _Segments


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



featureInterval :: forall a. Feature a -> Pair Bp
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
bumpFeatures f l radius other = map bump
  where maxInRadius :: Pair Bp -> Number
        maxInRadius lr = fromMaybe 0.0 $ maximum
                          $ map (\g -> if pairsOverlap (featureInterval g) lr
                                          then f `view` g else 0.0) other

        bump :: Record (FeatureR i) -> Record (FeatureR o)
        bump a =
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


type VScaleRow r = ( min :: Number
                   , max :: Number
                   , sig :: Number
                   | r )

type VScale = { width :: Number
              , color :: Color
              | VScaleRow () }

drawVScale :: forall r.
              VScale
           -> Number
           -> Drawing
drawVScale vscale height =
  -- should have some padding here too; hardcode for now
  let hPad = vscale.width  / 8.0
      vPad = height / 10.0
      x = 7.0 * hPad

      y1 = 0.0
      y2 = height

      n = (_ * 0.1) <<< Int.toNumber <$> (0 .. 10)

      p = Drawing.path [{x, y:y1}, {x, y:y2}]

      bar w y = Drawing.path [{x:x-w, y}, {x, y}]

      ps = foldMap (\i -> bar
                          (if i == 0.0 || i == 1.0 then 8.0 else 3.0)
                          (y1 + i*(y2-y1))) n

      ft = font sansSerif 10 mempty
      mkT y = Drawing.text ft hPad y (fillColor black)

      topLabel = mkT (y1+5.0) $ show vscale.max
      btmLabel = mkT (y2+5.0)       $ show vscale.min

  in outlined (outlineColor vscale.color <> lineWidth 2.0) (p <> ps)
     <> topLabel <> btmLabel


type LegendEntry = { text :: String, icon :: Drawing }

type Legend = { width :: Number
              , entries :: Array LegendEntry }



mkIcon :: Color -> String -> LegendEntry
mkIcon c text =
  let sh = circle (-2.5) (-2.5) 5.0
      icon = outlined (outlineColor c <> lineWidth 2.0) sh <>
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


getData :: forall a i c.
           CoordSys i c
        -> (CoordSys i c -> Json -> Maybe a)
        -> String
        -> Aff _ (Array a)
getData cs p url = do
  json <- _.response <$> Affjax.get url

  maybe (throwError $ error $ "Error when parsing features from " <> url)
        pure
        $ json ^? _Array >>= traverse (p cs)



getDataGrouped :: forall a i c.
                  CoordSys ChrId c
               -> (CoordSys ChrId c -> Json -> Maybe _)
               -> String
               -> Aff _ (Map ChrId (Array _))
getDataGrouped cs p url = groupToChrs <$> getData cs p url



eqLegend a b = a.text == b.text

trackLegend :: forall f a.
               Foldable f
            => Functor f
            => (a -> LegendEntry)
            -> f a
            -> Array LegendEntry
trackLegend f as = Array.nubBy eqLegend $ Array.fromFoldable $ map f as


data Track a = Track (Renderer a) (Map ChrId (Array a))


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

scalePoint :: Number
           -> Pair Number
           -> Normalized Point
           -> Point
scalePoint height seg@(Pair l _) np =
  let x = (unwrap np).x * pairSize seg + l
      y = height * (one - (unwrap np).y)
  in { x,y }



rescaleNormBatchGlyphs :: Number
                       -> Pair Number
                       -> BatchGlyph (Normalized Point)
                       -> BatchGlyph Point
rescaleNormBatchGlyphs height seg bg@{points} =
  bg { points = map (scalePoint height seg) points }

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





renderNormalizedTrack :: CoordSys ChrId BigInt
                      -> Canvas.Dimensions
                      -> Pair BigInt
                      -> Map ChrId (Either
                                      (BatchGlyph (Normalized Point))
                                      (Array NormalizedGlyph))
                      -- TODO concatting the segments with array is just lazy but works for now
                      --      optimally, a whole track should be one or the other type of glyph
                      -> Array (Either
                                  (BatchGlyph Point)
                                  (Array SingleGlyph))
renderNormalizedTrack cs cdim bView ngs =
  let segs :: Map ChrId (Pair Number)
      segs = scaledSegments cs { screenWidth: cdim.width, viewWidth: pairSize bView }

      renderSeg :: ChrId -> Pair Number -> Array (Either (BatchGlyph Point) (Array SingleGlyph))
      renderSeg k seg =
        pure $ fromMaybe (pure [])
               $ bimap
                 (rescaleNormBatchGlyphs  cdim.height seg)
                 (rescaleNormSingleGlyphs cdim.height seg)
                 <$> Map.lookup k ngs

  in foldMapWithIndex renderSeg segs


type UISlot = { offset :: Point
              , size   :: Canvas.Dimensions }

type UISlots = { left   :: UISlot
               , right  :: UISlot
               , top    :: UISlot
               , bottom :: UISlot }

type RenderedTrack = Either (BatchGlyph Point) (Array SingleGlyph)

browser :: CoordSys ChrId BigInt
        -> Canvas.Dimensions
        -> Canvas.Dimensions
        -> UISlots
        -> { legend :: Legend, vscale :: VScale }
        -> List (Exists Track)
        -> { tracks     :: Pair BigInt -> List (Array RenderedTrack)
           , relativeUI :: Pair BigInt -> Drawing
           , fixedUI    :: Drawing }
browser cs trackDim overlayDim uiSlots ui inputTracks =
  let
      drawInSlot {offset, size} d =
          (translate offset.x offset.y
           $ filled (fillColor white)
           $ rectangle zero zero size.width size.height)
        <> translate offset.x offset.y d

      vScale = drawInSlot uiSlots.left (drawVScale ui.vscale uiSlots.left.size.height)

      legend = drawInSlot uiSlots.right (drawLegend ui.legend uiSlots.right.size.height)

      ruler   = Drawing.translate ui.vscale.width zero
                $ horRulerTrack ui.vscale red trackDim

      fixedUI = ruler <> vScale <> legend

      normTracks :: List (Map ChrId
                          (Either (BatchGlyph (Normalized Point))
                                  (Array NormalizedGlyph)))
      normTracks = runExists (\(Track r as) -> render r <$> as) <$> inputTracks

      tracks :: Pair BigInt -> List (Array (Either (BatchGlyph Point) (Array SingleGlyph)))
      tracks v = (renderNormalizedTrack cs trackDim v) <$> normTracks


      renderUIElement :: Map ChrId (Array NormalizedGlyph)
                      -> ChrId -> Pair Number -> Array SingleGlyph
      renderUIElement m k s
          = fold $ rescaleNormSingleGlyphs trackDim.height s
                <$> (Map.lookup k m)

      drawTrackUI :: Pair BigInt -> (ChrId -> Pair Number -> (Array _)) -> Drawing
      drawTrackUI v = foldMap f <<< withPixelSegments cs trackDim v
        where f {drawing, point} = Drawing.translate point.x point.y (drawing unit)

      chrLabels :: _
      chrLabels = renderUIElement $ chrLabelTrack cs trackDim

      relativeUI :: Pair BigInt -> Drawing
      relativeUI v = drawTrackUI v chrLabels

  in { tracks
     , relativeUI
     , fixedUI
     }







--------------------------





type Feature' a = { segmentSize :: Bp
                  , pos         :: Pair Bp
                  , feature     :: a }



type DrawingN = { drawing :: Drawing, points :: Array Point }




type Rendered a = { features :: Array a
                  , drawings :: Array DrawingN
                  , overlaps :: Number -> Point -> Array a }


type Render a =
     Canvas.Dimensions
  -> Array a
  -> Pair Number
  -> { features :: Array a
     , drawings :: Array DrawingN
     , overlaps :: Number -> Point -> Array a }



type SNPFeature r = Feature' { score :: Number | r }


placeSNP :: forall r1 r2.
            { min :: Number, max :: Number | r1 }
         -> SNPFeature r2
         -> NPoint
placeSNP {min, max} { segmentSize, pos: (Pair l _), feature } = {x, y}
  where x = Normalized $ unwrap $ l / segmentSize
        y = Normalized $ (feature.score - min) / (max - min)


dist :: Point -> Point -> Number
dist p1 p2 = Math.sqrt $ x' `Math.pow` 2.0 + y' `Math.pow` 2.0
  where x' = p1.x - p2.x
        y' = p1.y - p2.y


-- can be used with mapWithIndex'd and scaledSegments',
-- to produce a `Map ChrId { features, drawings, overlaps }`
renderSNP :: forall r.
             { min :: Number, max :: Number | r }
          -> Canvas.Dimensions
          -> Array (SNPFeature ())
          -> Pair Number
          -> Rendered (SNPFeature ())
renderSNP verscale cdim snps =
  let features = snps

      radius = 2.2

      drawing =
          let color = navy
              c = circle 0.0 0.0 radius
              out = outlined (outlineColor color) c
              fill = filled (fillColor color) c
          in out <> fill

      drawings :: Array (Tuple (SNPFeature ()) Point) -> Array DrawingN
      drawings pts = let (Tuple _ points) = Array.unzip pts
                     in [{ drawing, points }]

      npointed :: Array (Tuple (SNPFeature ()) NPoint)
      npointed = map (fanout id (placeSNP verscale)) snps

      scale :: CoordSysView -> ViewScale
      scale csv = viewScale cdim csv

      rescale :: Pair Number -> NPoint -> Point
      rescale seg npoint =
        let (Pair offset _) = seg
            x = offset + (pairSize seg) * (unwrap npoint.x)
            y = cdim.height * unwrap npoint.y
        in {x, y}

      pointed :: Pair Number -> Array (Tuple (SNPFeature ()) Point)
      pointed seg = (map <<< map) (rescale seg) npointed

      overlaps :: Array (Tuple (SNPFeature ()) Point)
               -> Number -> Point
               -> Array (SNPFeature ())
      overlaps pts radius' pt = filterMap covers pts
        where covers :: Tuple (SNPFeature ()) Point -> Maybe (SNPFeature ())
              covers (Tuple f fPt) =
                if dist fPt pt <= radius' then Just f else Nothing


  in \seg -> let pts = pointed seg
             in { features
                , drawings: drawings pts
                , overlaps: overlaps pts }



renderSNPTrack :: forall r.
                  CoordSys ChrId BigInt
               -> { min :: Number, max :: Number | r }
               -> Canvas.Dimensions
               -> Map ChrId (Array (SNPFeature ()))
               -> Pair BigInt
               -> Map ChrId (Rendered (SNPFeature ()))
renderSNPTrack cs verscale cdim segSnps =
  let segs bView = scaledSegments cs { screenWidth: cdim.width, viewWidth: pairSize bView }
      midStep = map (renderSNP verscale cdim) segSnps
  in \bView -> zipMapsWith ($) midStep (segs bView)


renderTrack :: forall a.
               CoordSys ChrId BigInt
            -> Canvas.Dimensions
            -> Render a
            -> Map ChrId (Array a)
            -> Pair BigInt
            -> Map ChrId (Rendered a)
renderTrack cs cdim render segFs =
  let segs bView = scaledSegments cs { screenWidth: cdim.width, viewWidth: pairSize bView }

      midStep = map (render cdim) segFs

  in \bView -> zipMapsWith ($) midStep (segs bView)
