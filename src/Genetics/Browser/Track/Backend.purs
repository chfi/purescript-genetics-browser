module Genetics.Browser.Track.Backend where

import Prelude

import Color (Color, black, white)
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
import Data.Foldable (class Foldable, fold, foldMap, foldl, length, maximum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Lens (Getter', view, (^?))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (unwrap)
import Data.Pair (Pair(..))
import Data.Record as Record
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Data.Traversable (traverse)
import Data.Tuple (Tuple, snd, uncurry)
import Data.Variant (Variant, case_, inj, onMatch)
import Genetics.Browser.Types (Bp, ChrId, Point)
import Genetics.Browser.Types.Coordinates (CoordSys, Normalized(Normalized), pairSize, pairsOverlap, scaledSegments)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Network.HTTP.Affjax as Affjax
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


type ChrCtx r = Map ChrId (Record r)





_point = SProxy :: SProxy "point"
_range = SProxy :: SProxy "range"

type GenomePosV = Variant ( point :: Bp
                          , range :: Pair Bp )

type FeatureR r = ( position :: GenomePosV
                  , frameSize :: Bp
                  | r )
type Feature r = Record (FeatureR r)


type DrawingR  = ( point :: Drawing
                 , range :: Number -> Drawing )
type DrawingV  = Variant DrawingR

type HorPlaceR = ( point :: Normalized Number
                 , range :: Pair (Normalized Number) )
type HPos = Variant HorPlaceR



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


type BatchGlyph c = { drawing :: Drawing
                    , points :: Array c }

type UniqueGlyph = { drawing :: Drawing, point :: Point }

type BatchableGlyph = { drawing :: Drawing
                      , points :: Array (Normalized Point) }


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
verPlace = view


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


type FixedUIComponent    = CanvasReadyDrawing
type RelativeUIComponent = Pair BigInt -> CanvasReadyDrawing


-- type Browser = { tracks     :: Array _
--                , relativeUI :: Array RelativeUIComponent
--                , fixedUI    :: Array FixedUIComponent }


type CanvasSingleGlyph = { drawing :: Drawing, point :: Point }
type CanvasBatchGlyph  = { drawing :: Drawing, points :: Array Point }

type BatchedTrack = Array CanvasBatchGlyph

type CanvasReadyDrawing = Drawing

type BrowserTrack = Canvas.Dimensions
                 -> Pair BigInt
                 -> CanvasReadyDrawing


-- Given a frame-normalized drawing, finalize it so it can be rendered to a canvas;
-- `width` is the width in pixels of the frame the drawing resides in.
resizeDrawing :: forall r.
                 { width :: Number | r }
              -> DrawingV
              -> Drawing
resizeDrawing {width} = case_
  # onMatch
     { point: (\x -> x)
     , range: (_ $ width) }




horRulerTrack :: forall r.
                 { min :: Number, max :: Number, sig :: Number | r }
              -> Color
              -> BrowserTrack
horRulerTrack {min, max, sig} color f _ = outlined outline rulerDrawing
  where normY = (sig - min) / (max - min)
        y = f.height - (normY * f.height)
        outline = outlineColor color
        rulerDrawing = Drawing.path [{x: 0.0, y}, {x: f.width, y}]


-- chrLabelTrack :: CoordSys ChrId BigInt
--               -> Canvas.Dimensions
--               -> RelativeUIComponent
-- chrLabelTrack cs = unsafeCoerce unit
-- chrLabelTrack cs = drawRelativeUI cs (Map.fromFoldable results)
--   where mkLabel :: ChrId -> NormalizedGlyph
--         mkLabel chr = { drawing: inj _point $ chrText chr
--                       , horPos:  inj _point $ Normalized (0.5)
--                       , verPos: Normalized (0.05) }

--         font' = font sansSerif 12 mempty

--         chrText :: ChrId -> Drawing
--         chrText chr = Drawing.text font' zero zero (fillColor black) (unwrap chr)

--         results :: Array _
--         results = map (\x -> Tuple zero [mkLabel (x^._Index)]) $ cs^._BrowserIntervals


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
  let hPad = vscale.width  / 5.0
      vPad = height / 10.0
      x = vscale.width / 2.0

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

      topLabel = mkT (y1-(0.5*vPad)) $ show vscale.max
      btmLabel = mkT (y2+vPad)       $ show vscale.min

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


data Track a = Track (Map ChrId (Array a)) (Renderer a)


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
                    -> Drawing
finalizeNormDrawing seg o =
    case_
  # onMatch { point: \x -> x
            , range: (_ $ pairSize seg) }
  $ o.drawing

renderNormalized1 :: Number
                  -> Pair Number
                  -> NormalizedGlyph
                  -> UniqueGlyph
renderNormalized1 height seg@(Pair l _) ng =
  let x = horPlaceOnSegment seg ng + l
      y = height * (one - unwrap ng.verPos)
      drawing = finalizeNormDrawing seg ng
  in { point: {x,y} , drawing }

scalePoint :: Number
           -> Pair Number
           -> Normalized Point
           -> Point
scalePoint height seg@(Pair l _) np =
  let x = (unwrap np).x * pairSize seg + l
      y = height * (one - (unwrap np).y)
  in { x,y }



renderNormalizedTrack :: CoordSys ChrId BigInt
                      -> Canvas.Dimensions
                      -> Pair BigInt
                      -> Map ChrId (Either
                                      (BatchGlyph (Normalized Point))
                                      (Array NormalizedGlyph))
                      -> Map ChrId (Either
                                      (BatchGlyph Point)
                                      (Array UniqueGlyph))
renderNormalizedTrack cs cdim bView ngs =
  let segs :: Map ChrId (Pair Number)
      segs = scaledSegments cs { screenWidth: cdim.width, viewWidth: pairSize bView }

      batchT :: Pair Number -> BatchGlyph (Normalized Point) -> BatchGlyph Point
      batchT seg bg@{points} = bg { points = map (scalePoint cdim.height seg) points }

      uniqT :: Pair Number -> Array NormalizedGlyph -> Array UniqueGlyph
      uniqT seg = map (renderNormalized1 cdim.height seg)

      renderSeg :: ChrId -> Pair Number -> Either (BatchGlyph Point) (Array UniqueGlyph)
      renderSeg k seg = fromMaybe (pure [])
                         $ bimap (batchT seg) (uniqT seg)
                        <$> Map.lookup k ngs

  in mapWithIndex renderSeg segs



browser :: CoordSys ChrId BigInt
        -> Canvas.Dimensions
        -> Padding
        -> { legend :: Legend, vscale :: VScale }
        -> List (Exists Track)
        -> { tracks     :: Pair BigInt -> List _
           , relativeUI :: Pair BigInt -> Drawing
           , fixedUI    :: Drawing }
browser cs cdim padding ui inputTracks =
  let height = cdim.height - 2.0 * padding.vertical
      width  = cdim.width

      trackCanvas = { width: width - ui.vscale.width - ui.legend.width
                    , height }

      -- TODO make a type that corresponds to left-of-track and right-of-track to make this dynamic
      drawOverlay x w d =
        (translate x zero
         $ filled (fillColor white)
         $ rectangle zero zero w height )
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


      fixedUI = vScale <> legend

      normTracks :: List (Map ChrId
                          (Either (BatchGlyph (Normalized Point))
                                  (Array NormalizedGlyph)))
      normTracks = runExists (\(Track as r) -> render r <$> as) <$> inputTracks

      -- tracks :: Pair BigInt -> List (Either (BatchGlyph Point) (Array UniqueGlyph))
      tracks :: Pair BigInt -> _
      tracks v = (renderNormalizedTrack cs cdim v) <$> normTracks

      -- chrLabels = chrLabelTrack cs trackCanvas
      -- ruler     = horRulerTrack ui.vscale red trackCanvas
      -- boxes     = boxesTrack height cs trackCanvas

      -- relativeUI = chrLabels
      --           <> translate 0.0 padding.vertical <<< (ruler <> boxes)

  in { tracks: mempty
     , relativeUI: mempty
     , fixedUI
     }
