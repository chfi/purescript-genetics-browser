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
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Lens ((^.))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number.Format as Num
import Data.Pair (Pair(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Variant (inj)
import Foreign (F, Foreign, ForeignError(..), readString)
import Genetics.Browser.Canvas (ContentLayer, Renderable, RenderableLayer, _drawing)
import Genetics.Browser.Coordinates (CoordSys, CoordSysView, Normalized, aroundPair, normalize, scaledSegments, scaledSegments', viewScale, xPerPixel)
import Genetics.Browser.Layer (Component(CBottom), Layer(Layer), LayerMask(NoMask, Masked), LayerType(Fixed, Scrolling))
import Genetics.Browser.Types (Bp, ChrId, _exp)
import Graphics.Canvas (Dimensions) as Canvas
import Graphics.Drawing (Drawing, Shape, fillColor, filled, lineWidth, outlineColor, outlined, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Math (pow)
import Math as Math
import Prim.Row as Row
import Record (get)
import Simple.JSON (class ReadForeign)
import Type.Prelude (class IsSymbol, SProxy)


type Feature a = { position  :: Pair Bp
                 , frameSize :: Bp
                 , feature   :: a }


featureNormX :: ∀ a. Feature a -> Normalized Number
featureNormX { frameSize, position: (Pair l _) } =
  wrap $ unwrap (l / frameSize)


type TrackLike a =
     Map ChrId (Pair Number)
  -> Canvas.Dimensions
  -> a

type Peak x y r = { covers :: Pair x
                  , y :: y
                  , elements :: Array r }


groupToMap :: ∀ i a f.
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


pixelSegments :: ∀ r c.
                 { segmentPadding :: Number | r }
              -> CoordSys c BigInt
              -> Canvas.Dimensions
              -> CoordSysView
              -> Map c (Pair Number)
pixelSegments conf cSys trackDim csView =
  aroundPair (-conf.segmentPadding)
       <$> scaledSegments cSys (viewScale trackDim csView)


pixelSegmentsOpt :: ∀ r i.
                  Ord i
               => { segmentPadding :: Number | r }
              -> CoordSys i BigInt
              -> Canvas.Dimensions
              -> CoordSysView
              -> Map i (Pair Number)
pixelSegmentsOpt conf cSys trackDim csView =
  aroundPair (-conf.segmentPadding)
       <$> (scaledSegments' cSys csView (viewScale trackDim csView))




trackLikeLayer :: ∀ a b l rC r1 r2.
                  IsSymbol l
               => Row.Cons  l b r1 ( view :: CoordSysView | r2 )
               => { segmentPadding :: Number
                  , coordinateSystem :: CoordSys ChrId BigInt | rC }
               -> SProxy l
               -> Component (b -> Map ChrId (Pair Number) -> Canvas.Dimensions -> a)
               -> Layer (Tuple (Record ( view :: CoordSysView | r2 ))
                               Canvas.Dimensions
                      -> a)
trackLikeLayer conf name com =
  let segs :: Canvas.Dimensions -> CoordSysView -> Map ChrId (Pair Number)
      segs = pixelSegments conf conf.coordinateSystem

      fun :: (b -> Map ChrId (Pair Number) -> Canvas.Dimensions -> a) -> Tuple _ _ -> a
      fun f (Tuple r d) = f (get name r) (segs d r.view) d

      com' :: Component (Tuple (Record (view :: CoordSysView | r2)) Canvas.Dimensions -> a)
      com' = fun <$> com

  in Layer Scrolling (Masked 5.0) com'



renderFixedUI :: ∀ c.
                 Component (Canvas.Dimensions -> Drawing)
              -> RenderableLayer c
renderFixedUI com = Layer Fixed NoMask $ map f com
  where f draw = \_ d -> pure $ inj _drawing (draw d)


fixedUILayer :: ∀ a.
                Component (Canvas.Dimensions -> Drawing)
             -> Layer (Tuple a Canvas.Dimensions
                       -> { renderables :: List Renderable })
fixedUILayer com = Layer Fixed NoMask $ map f com
  where f draw = \(Tuple _ d) -> { renderables: pure $ inj _drawing (draw d) }



thresholdRuler :: ∀ r r1.
                  Tuple
                  { threshold :: Record (VScaleRow r1)
                  , rulerColor :: HexColor | r }
                  Canvas.Dimensions
               -> { renderables :: List Renderable }
thresholdRuler (Tuple {threshold: {sig,min,max}, rulerColor} slot) =
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

  in { renderables: List.fromFoldable
       [ inj _drawing rulerDrawing
       , inj _drawing label ] }


chrLabelsLayer :: ∀ r1 r2 r3.
                  { segmentPadding :: Number
                  , coordinateSystem :: CoordSys ChrId BigInt | r3 }
               -> { fontSize :: Int | r1 }
               -> Layer (Tuple _ Canvas.Dimensions -> { renderables :: List Renderable })
chrLabelsLayer trackConf {fontSize} =
  let
      labelOffset chrId = 0.3 *
        (Int.toNumber $ fontSize * (String.length $ show chrId))

      labelSeg :: Canvas.Dimensions
               -> CoordSysView
               -> Tuple ChrId (Pair Number)
               -> Drawing
      labelSeg d v (Tuple c s@(Pair l r)) =
          Drawing.text
             (font sansSerif fontSize mempty)
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


  in Layer Scrolling (Masked 5.0)
     $ CBottom \(Tuple {view} dim) ->
         { renderables:
           map (inj _drawing <<< labelSeg dim view)
           $ Map.toUnfoldable
           $ pixelSegments trackConf trackConf.coordinateSystem dim view
         }



chrBackgroundLayer :: ∀ r.
                      { chrBG1 :: HexColor
                      , chrBG2 :: HexColor
                      , segmentPadding :: Number | r }
                   -> Map ChrId (Pair Number)
                   -> Canvas.Dimensions
                   -> { renderables :: List Renderable }
chrBackgroundLayer conf seg size =
  let col c = if c then black else gray

      segBG :: Pair Number -> State Boolean (Tuple Color Shape)
      segBG (Pair l r) = do
        curSeg <- State.get
        State.modify_ not
        pure $ Tuple
          (unwrap $ if curSeg then conf.chrBG1
                              else conf.chrBG2)
          (Drawing.rectangle
             (l   -     conf.segmentPadding) (-5.0)
             (r-l + 2.0*conf.segmentPadding) (size.height+10.0))

  in { renderables:
       map (inj _drawing
          <<< uncurry \c s -> filled (fillColor c) s)
     $ evalState (traverse segBG $ Map.values seg) false }


newtype HexColor = HexColor Color

derive instance newtypeHexColor :: Newtype HexColor _

instance readforeignHexColor :: ReadForeign HexColor where
  readImpl = map wrap <$> parseColor


parseColor :: Foreign -> F Color
parseColor = readColor <=< readString
  where readColor c =
          except
           $ note (pure $ ForeignError "Could not parse color: expected hex string")
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


trackLegend :: ∀ f a.
               Foldable f
            => Functor f
            => (a -> LegendEntry)
            -> f a
            -> Array LegendEntry
trackLegend f as = Array.nubBy (compare `on` _.text) $ Array.fromFoldable $ map f as
