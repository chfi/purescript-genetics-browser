module Genetics.Browser where

import Prelude

import Color (Color, black)
import Color as Color
import Color.Scheme.Clrs (gray)
import Control.Monad.Except (except)
import Control.Monad.State (State, evalState)
import Control.Monad.State.Class as State
import Data.Array ((..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (note)
import Data.Foldable (class Foldable, fold, foldMap, foldl, length)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foreign (F, Foreign, ForeignError(..), readString)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Lens ((^.))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number.Format as Num
import Data.Pair (Pair(..))
import Data.Record (get)
import Data.String as String
import Data.Symbol (SProxy)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Variant (Variant, case_, inj, onMatch)
import Genetics.Browser.Canvas (Renderable, RenderableLayer, RenderableLayerHotspots, _static)
import Genetics.Browser.Coordinates (CoordSys, CoordSysView, Normalized(Normalized), aroundPair, normalize, pairSize, scaledSegments, viewScale, xPerPixel)
import Genetics.Browser.Layer (Component(Padded, Full, CBottom), Layer(Layer), LayerMask(NoMask, Masked), LayerType(Fixed, Scrolling))
import Genetics.Browser.Types (Bp, ChrId, _exp)
import Graphics.Canvas (Dimensions) as Canvas
import Graphics.Drawing (Drawing, Point, Shape, fillColor, filled, lineWidth, outlineColor, outlined, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Math (pow)
import Math as Math
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign)
import Type.Prelude (class IsSymbol)



type Feature a = { position  :: Pair Bp
                 , frameSize :: Bp
                 , feature   :: a }

featureNormX :: Feature _ -> Normalized Number
featureNormX { frameSize, position: (Pair l _) } = wrap $ unwrap (l / frameSize)



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



thresholdRuler :: ∀ r r1.
                  { threshold :: Record (VScaleRow r1)
                  , rulerColor :: HexColor | r }
               -> Canvas.Dimensions
               -> List Renderable
thresholdRuler {threshold: {sig,min,max}, rulerColor} slot =
  let y = slot.height - (normalize min max sig * slot.height)

      outline = outlineColor (unwrap rulerColor)
                <> lineWidth 2.0

      rulerDrawing =
        outlined outline
        $ Drawing.path [{x: -5.0, y}, {x: slot.width+5.0, y}]

      text  = "P = " <> (10.0 `pow` -sig) ^. _exp 1
      font' = font sansSerif 16 mempty
      label = Drawing.text
              font'
              (slot.width+10.0) (y-6.0)
              (fillColor $ unwrap rulerColor) text

  in List.fromFoldable
       [ inj _static rulerDrawing
       , inj _static label ]


chrLabels :: ∀ r1 r2 a.
               { segmentPadding :: Number
               , fontSize :: Int | r1 }
            -> CoordSys ChrId BigInt
            -> RenderableLayer { view :: CoordSysView | r2 }
chrLabels conf cSys =
  let
      labelOffset chrId = 0.3 *
        (Int.toNumber $ conf.fontSize * (String.length $ show chrId))

      labelSeg :: Canvas.Dimensions
               -> CoordSysView
               -> Tuple ChrId (Pair Number)
               -> Drawing
      labelSeg d v (Tuple c s@(Pair l r)) =
          Drawing.text
             (font sansSerif conf.fontSize mempty)
             (segMidPoint (viewPixels d v) s - labelOffset c)
             (0.7 * d.height)
             (fillColor black)
             (show c)

      viewPixels :: Canvas.Dimensions -> CoordSysView -> Pair Number
      viewPixels d v = let s = viewScale d v
                           v' = map BigInt.toNumber $ unwrap v
                       in (_ / xPerPixel s) <$> v'

      segMidPoint :: Pair Number
                  -> Pair Number
                  -> Number
      segMidPoint (Pair vL vR) (Pair sL sR) =
        let l' = max vL sL
            r' = min vR sR
        in l' + ((r' - l') / 2.0)


  in Layer Scrolling Masked
     $ CBottom \ {view} dim ->
         map (inj _static <<< labelSeg dim view)
         $ Map.toUnfoldable
         $ pixelSegments conf cSys dim view


chrBackgroundLayer :: ∀ r a.
                      { bg1 :: HexColor
                      , bg2 :: HexColor
                      , segmentPadding :: Number | r }
                   -> Map ChrId (Pair Number)
                   -> Canvas.Dimensions
                   -> List Renderable
chrBackgroundLayer conf seg size =
  let col c = if c then black else gray

      segBG :: Pair Number -> State Boolean (Tuple Color Shape)
      segBG (Pair l r) = do
        curSeg <- State.get
        State.modify not
        pure $ Tuple
          (unwrap $ if curSeg then conf.bg1
                              else conf.bg2)
          (Drawing.rectangle
             (l   -     conf.segmentPadding) (-5.0)
             (r-l + 2.0*conf.segmentPadding) (size.height+10.0))

  in map (inj _static
          <<< uncurry \c s -> filled (fillColor c) s)
     $ evalState (traverse segBG $ Map.values seg) false


newtype HexColor = HexColor Color

derive instance newtypeHexColor :: Newtype HexColor _

instance readforeignHexColor :: ReadForeign HexColor where
  readImpl = map wrap <$> parseColor


parseColor :: Foreign -> F Color
parseColor = readColor <=< readString
  where readColor c =
          except
           $ note (pure $ JSONError "Could not parse color: expected hex string")
           $ Color.fromHexString c


type VScaleRow r = ( min :: Number
                   , max :: Number
                   , sig :: Number
                   | r )

type Threshold = Record (VScaleRow ())

type VScale r = { color :: HexColor
                , hPad :: Number
                , numSteps :: Int
                , fonts :: { scaleSize :: Int
                           , labelSize :: Int }
                | r }


defaultVScaleConfig :: VScale ()
defaultVScaleConfig =
  { color: wrap black
  , hPad: 0.125
  , numSteps: 3
  , fonts: { labelSize: 18, scaleSize: 14 } }


drawVScaleInSlot :: VScale (VScaleRow ())
                 -> Canvas.Dimensions
                 -> Drawing
drawVScaleInSlot vscale size =
  let
      -- TODO expose linewidth config
      -- TODO expose offsets in config
      hPad = size.width * vscale.hPad
      numSteps = vscale.numSteps

      x = 7.0 * hPad

      spokes = (_ / (Int.toNumber numSteps))
               <<< Int.toNumber <$> (0 .. numSteps)

      barOutline = outlineColor (unwrap vscale.color) <> lineWidth 2.0

      vBar = Drawing.path [{x, y: 0.0}, {x, y: size.height}]

      hBar w y = Drawing.path [{x:x-w, y}, {x, y}]
      bars = vBar
          <> foldMap (\i -> hBar 8.0 (i * size.height)) spokes

      unitLabel =
        Drawing.translate (size.width * 0.4 - hPad) (size.height * 0.72)
        $ Drawing.rotate (- Math.pi / 2.0)
        $ Drawing.text (font sansSerif vscale.fonts.labelSize mempty)
            0.0 0.0 (fillColor black) "-log10 (P value)"

      label yN = Drawing.text
                    (font sansSerif vscale.fonts.scaleSize mempty)
                    (size.width * 0.6 - hPad)
                    (yN * size.height + 5.0)
                    (fillColor black)
                 $ Num.toStringWith (Num.fixed 0)
                 $ (\p -> min vscale.max p)
                 $ vscale.min + (1.0 - yN) * (vscale.max - vscale.min)

      labels = foldMap label spokes

  in outlined barOutline bars <> labels <> unitLabel



type LegendEntry = { text :: String, icon :: Drawing }

type Legend = { entries :: Array LegendEntry }


drawLegendItem :: LegendEntry
               -> Drawing
drawLegendItem {text, icon} =
  let ft = font sansSerif 12 mempty
      t = Drawing.text ft 12.0 0.0 (fillColor black) text
  in icon <> t


type LegendConfig r =
  { hPad :: Number, vPad :: Number
  , fontSize :: Int | r }

defaultLegendConfig :: LegendConfig ()
defaultLegendConfig =
  { hPad: 0.2, vPad: 0.2
  , fontSize: 12 }



drawLegendInSlot :: ∀ r.
                    LegendConfig (entries :: Array LegendEntry | r)
                 -> Canvas.Dimensions
                 -> Drawing
drawLegendInSlot c@{entries} size =
  let
      hPad = size.width  * c.hPad
      vPad = size.height * c.vPad

      x = hPad

      font' = font sansSerif c.fontSize mempty

      drawEntry :: Number -> { text :: String, icon :: Drawing } -> Drawing
      drawEntry y {text, icon} =
        translate x y
          $ icon <> Drawing.text font' 12.0 0.0 (fillColor black) text

      d = (size.height - 2.0*vPad) / length entries
      ds = mapWithIndex (\i ic -> drawEntry (vPad*(Int.toNumber (i+1) )) ic) entries
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



trackLegend :: forall f a.
               Foldable f
            => Functor f
            => (a -> LegendEntry)
            -> f a
            -> Array LegendEntry
trackLegend f as = Array.nubBy (eq `on` _.text) $ Array.fromFoldable $ map f as



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
  let scale = viewScale cdim (wrap bView)
  in flip foldMapWithIndex (scaledSegments cs scale)


--------------------------


type DrawingN = { drawing :: Drawing, points :: Array Point }

-- TODO this will be handled much more nicely using type classes
type TrackRenderer =
     Map ChrId (Pair Number)
  -> Canvas.Dimensions
  -> List Renderable

type TrackHotspotsRenderer a =
     Map ChrId (Pair Number)
  -> Canvas.Dimensions
  -> { renderables :: List Renderable
     , hotspots :: Number -> Point -> Array a }

type Renderer a =
     Map ChrId (Array a)
  -> Map ChrId (Pair Number)
  -> Canvas.Dimensions
  -> List Renderable


pixelSegments :: ∀ r c.
                 { segmentPadding :: Number | r }
              -> CoordSys c BigInt
              -> Canvas.Dimensions
              -> CoordSysView
              -> Map c (Pair Number)
pixelSegments conf cSys trackDim csView =
  aroundPair (-conf.segmentPadding)
       <$> scaledSegments cSys (viewScale trackDim csView)


renderHotspots :: ∀ a b l rC r1 r2.
               IsSymbol l
            => RowCons  l b r1 ( view :: CoordSysView | r2 )
            => { segmentPadding :: Number | rC }
            -> CoordSys ChrId BigInt
            -> SProxy l
            -> Component (b -> TrackHotspotsRenderer a)
            -> RenderableLayerHotspots (Record ( view :: CoordSysView | r2 )) a
renderHotspots conf cSys name com =
  let
      segs :: Canvas.Dimensions -> CoordSysView -> Map ChrId (Pair Number)
      segs = pixelSegments conf cSys
  in case com of
        Full     r ->
          Layer Scrolling NoMask
            $ Full     \c d -> r (get name c) (segs d c.view) d

        Padded p r ->
          Layer Scrolling Masked
            $ Padded p \c d -> r (get name c) (segs d c.view) d

        _ -> unsafeCrashWith "renderTrack' does not support UI slots yet"

renderTrack :: ∀ a b l rC r1 r2.
               IsSymbol l
            => RowCons  l b r1 ( view :: CoordSysView | r2 )
            => { segmentPadding :: Number | rC }
            -> CoordSys ChrId BigInt
            -> SProxy l
            -> Component (b -> TrackRenderer)
            -> RenderableLayer (Record ( view :: CoordSysView | r2 ))
renderTrack conf cSys name com =
  let
      segs :: Canvas.Dimensions -> CoordSysView -> Map ChrId (Pair Number)
      segs = pixelSegments conf cSys
  in case com of
        Full     r ->
          Layer Scrolling NoMask
            $ Full     \c d -> r (get name c) (segs d c.view) d

        Padded p r ->
          Layer Scrolling Masked
            $ Padded p \c d -> r (get name c) (segs d c.view) d

        _ -> unsafeCrashWith "renderTrack' does not support UI slots yet"


renderFixedUI :: Component (Canvas.Dimensions -> Drawing)
              -> RenderableLayer Unit
renderFixedUI com = Layer Fixed NoMask $ map f com
  where f draw = \_ d -> pure $ inj _static (draw d)




type Peak x y r = { covers :: Pair x
                  , y :: y
                  , elements :: Array r }
