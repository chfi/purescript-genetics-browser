module Genetics.Browser.Track.Backend where
       -- ( demoBrowser
       -- , demoLegend
       -- , getDataDemo
       -- ) where

import Prelude

import Color (Color, black, white)
import Color.Scheme.Clrs (aqua, blue, fuchsia, green, lime, maroon, navy, olive, orange, purple, red, teal, yellow)
import Control.Alternative (class Alternative, alt, empty)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (partition, partitioned)
import Data.Foldable (class Foldable, fold, foldMap, foldl, foldr, length, maximum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Lens (Getter', Traversal, Traversal', over, re, to, traverseOf, traversed, view, viewOn, (^.), (^?))
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Pair (Pair(..))
import Data.Profunctor.Star (Star(..))
import Data.Record as Record
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(Tuple), snd, uncurry)
import Data.Unfoldable (unfoldr)
import Data.Variant (Variant, case_, inj, on, onMatch, prj)
import Debug.Trace as Debug
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId), Point)
import Genetics.Browser.Types.Coordinates (BrowserPoint, CoordSys, Interval, Normalized(Normalized), _BrowserIntervals, _Index, _Interval, intervalToScreen, intervalsOverlap, intervalsToMap, lookupInterval)
import Genetics.Browser.View (Pixels)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Network.HTTP.Affjax as Affjax
import Type.Prelude (class RowLacks)
import Unsafe.Coerce (unsafeCoerce)


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


type Renderer a = { draw     :: a -> DrawingV
                  , horPlace :: a -> HPos
                  , verPlace :: a -> Normalized Number
                  }


type NormalizedGlyph = { drawing :: Variant DrawingR
                       , horPos  :: Variant HorPlaceR
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
verPlace = view

render :: forall err a.
          Renderer a
       -> a
       -> NormalizedGlyph
render render a =
      { drawing: render.draw a
      , horPos:  render.horPlace a
      , verPos:  render.verPlace a
      }


type BatchedDrawings = { drawing :: Drawing, points :: Array Point }
type SingleDrawing   = { drawing :: Drawing, point :: Point }


type Glyph = Variant ( batched :: BatchedDrawings
                     , single  :: SingleDrawing  )


type FixedUIComponent    = CanvasReadyDrawing
type RelativeUIComponent = Interval BrowserPoint -> CanvasReadyDrawing



type Browser = { tracks     :: Array Glyph
               , relativeUI :: Array RelativeUIComponent
               , fixedUI    :: Array FixedUIComponent }


type CanvasSingleGlyph = { drawing :: Drawing, point :: Point }
type CanvasBatchGlyph  = { drawing :: Drawing, points :: Array Point }

type BatchedTrack = Array CanvasBatchGlyph

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


_LHS :: Getter' HPos (Normalized Number)
_LHS = to $ case_
         # onMatch { point: \x -> x, range: \(Pair l _) -> l}


renderNormalizedGlyphs ::  forall f r a.
                          Canvas.Dimensions
                       -> { width :: Pixels, offset :: Pixels }
                       -> Array NormalizedGlyph
                       -> { toBatch :: Array SingleDrawing
                          , singles :: Array SingleDrawing }
renderNormalizedGlyphs {height} {width, offset} gs =
    (\ {left, right} -> {toBatch: left, singles: right})
    $ partitioned $ map drawIt gs

  where drawIt {drawing, horPos, verPos} =
          let x = width * unwrap (horPos ^. _LHS)
              y = height * (1.0 - unwrap verPos)
              d = resizeDrawing {width} drawing
              res = { drawing: d, point: { x: (x + offset), y } }
          in if isJust $ prj _point drawing
                then Left res else Right res


batchDrawings :: Array SingleDrawing
              -> Array BatchedDrawings
batchDrawings = unfoldr f
  where f gs = Array.uncons gs >>= \ {head, tail} ->
                 let { no, yes } = partition (\ {drawing} -> head.drawing == drawing) tail
                     points = Array.cons head.point $ map _.point yes
                 in pure $ Tuple { drawing: head.drawing, points } no


batchNormalizedGlyphs :: forall f r a.
                         Canvas.Dimensions
                      -> { width :: Pixels, offset :: Pixels }
                      -> Array NormalizedGlyph
                      -> Array Glyph
batchNormalizedGlyphs h wo gs =
  let {toBatch, singles} = renderNormalizedGlyphs h wo gs
      batched = inj (SProxy :: SProxy "batched") <$> batchDrawings toBatch
      single  = inj (SProxy :: SProxy "single")  <$> singles
  in batched <> single



drawIntervalFeature :: forall f r a.
                       Foldable f
                    => Canvas.Dimensions
                    -> { width :: Pixels, offset :: Pixels }
                    -> f NormalizedGlyph
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



drawRelativeUI :: forall f i a.
                  Ord i
               => Foldable f
               => CoordSys i BrowserPoint
               -> Map i (f NormalizedGlyph)
               -> Canvas.Dimensions
               -> RelativeUIComponent
drawRelativeUI cs drawings cd v =
  fold $ zipMapsWith (drawIntervalFeature cd) (viewportIntervals cs cd v) drawings


pureTrackGlyphs :: forall f i a.
                   Ord i
                => CoordSys i BrowserPoint
                -> Map i (Array NormalizedGlyph)
                -> Canvas.Dimensions
                -> Interval BrowserPoint
                -> Map i (Array Glyph)
pureTrackGlyphs cs glyphs cd v =
  zipMapsWith (\v gs -> batchNormalizedGlyphs cd v gs) (viewportIntervals cs cd v) glyphs



renderTrackGlyphs ::  forall f i a.
                      Ord i
                  => Foldable f
                  => Traversable f
                  => CoordSys i BrowserPoint
                  -> Renderer a
                  -> Map i (f a)
                  -> Canvas.Dimensions
                  -> Interval BrowserPoint
                  -> Array Glyph
renderTrackGlyphs cs r trackData =
  let data' = map Array.fromFoldable trackData
      glyphs = (map <<< map) (render r) data'
  in \cd v -> fold $ pureTrackGlyphs cs glyphs cd v


horRulerTrack :: forall r.
                 { min :: Number, max :: Number, sig :: Number | r }
              -> Color
              -> BrowserTrack
horRulerTrack {min, max, sig} color f _ = outlined outline rulerDrawing
  where normY = (sig - min) / (max - min)
        y = f.height - (normY * f.height)
        outline = outlineColor color
        rulerDrawing = Drawing.path [{x: 0.0, y}, {x: f.width, y}]


chrLabelTrack :: CoordSys ChrId BrowserPoint
              -> Canvas.Dimensions
              -> RelativeUIComponent
chrLabelTrack cs = drawRelativeUI cs (Map.fromFoldable results)
  where mkLabel :: ChrId -> NormalizedGlyph
        mkLabel chr = { drawing: inj _point $ chrText chr
                      , horPos:  inj _point $ Normalized (0.5)
                      , verPos: Normalized (0.05) }

        font' = font sansSerif 12 mempty

        chrText :: ChrId -> Drawing
        chrText chr = Drawing.text font' zero zero (fillColor black) (unwrap chr)

        results :: Array _
        results = map (\x -> Tuple (x^._Index) [mkLabel (x^._Index)]) $ cs^._BrowserIntervals


boxesTrack :: Number
           -> CoordSys ChrId BrowserPoint
           -> Canvas.Dimensions
           -> RelativeUIComponent
boxesTrack h cs = drawRelativeUI cs $ map (const [glyph]) $ intervalsToMap cs
  where glyph :: NormalizedGlyph
        glyph = { drawing: inj _range draw
                , horPos:  inj _range $ Normalized <$> (Pair zero one)
                , verPos: Normalized one }
        draw = \w ->
             let rect = rectangle 0.0 0.0 w h
             in outlined (outlineColor black <> lineWidth 1.5) rect



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
bumpFeatures f l radius other = map bump
  where maxInRadius :: Interval Bp -> Number
        maxInRadius lr = fromMaybe 0.0 $ maximum
                          $ map (\g -> if intervalsOverlap (featureInterval g) lr
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

type VScale = { width :: Pixels
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

type Legend = { width :: Pixels
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


type Padding = { vertical :: Pixels
               , horizontal :: Pixels
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



type TrackRow c a = ( url :: String
                    , parse :: c -> Json -> Maybe a
                    , render :: Renderer a )

type GWASTrack c a  = { vscale :: { | VScaleRow () }
                      | TrackRow c a }

type AnnotTrack c a = { getEntry :: a -> LegendEntry | TrackRow c a }


eqLegend a b = a.text == b.text

trackLegend :: forall f a.
               Foldable f
            => Functor f
            => (a -> LegendEntry)
            -> f a
            -> Array LegendEntry
trackLegend f as = Array.nubBy eqLegend $ Array.fromFoldable $ map f as
