module Genetics.Browser where

import Prelude

import Color (Color, black, white)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Reader (class MonadReader, ask)
import Data.Array ((..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Foldable (class Foldable, fold, foldMap, foldl, length)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Lens ((^.))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (unwrap, wrap)
import Data.Number.Format as Num
import Data.Pair (Pair(..))
import Data.Record as Record
import Data.Record.Builder (build, insert)
import Data.Record.Unsafe as Record
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Data.Variant (Variant, case_, inj, onMatch)
import Genetics.Browser.Cached (Cached, cache)
import Genetics.Browser.Canvas (BrowserCanvas, BrowserContainer(..), Label, Renderable, UISlot, UISlotGravity(UIBottom, UITop, UIRight, UILeft), RenderableLayer, _Dimensions, _Track, _static)
import Genetics.Browser.Canvas (uiSlots) as Canvas
import Genetics.Browser.Coordinates (CoordSys, CoordSysView, Normalized(Normalized), _Segments, aroundPair, pairSize, scaledSegments, viewScale)
import Genetics.Browser.Layer (Component(..), ComponentSlot, Layer(..), LayerMask(..), LayerType(..))
import Genetics.Browser.Types (Bp, ChrId, _exp)
import Graphics.Canvas (Dimensions) as Canvas
import Graphics.Drawing (Drawing, Point, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Math (pow)
import Math as Math
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (class RowLacks)
import Unsafe.Coerce (unsafeCoerce)


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


sigLevelRuler :: forall r.
                 { min :: Number, max :: Number, sig :: Number | r }
              -> Color
              -> Canvas.Dimensions
              -> Drawing
sigLevelRuler {min, max, sig} color f = outlined outline rulerDrawing <> label
  where normY = (sig - min) / (max - min)
        thickness = 2.0
        outline = outlineColor color <> lineWidth thickness
        y = thickness + f.height - (normY * f.height)
        rulerDrawing = Drawing.path [{x: 0.0, y}, {x: f.width, y}]

        text  = "P = " <> (10.0 `pow` -sig) ^. _exp 1

        font' = font sansSerif 16 mempty
        label = Drawing.text font' (f.width+4.0) (y-6.0) (fillColor color) text


chrLabelTrack :: ∀ r.
                 { fontSize :: Int | r }
              -> CoordSys ChrId BigInt
              -> Map ChrId (Array NormalizedGlyph)
chrLabelTrack {fontSize} cs =
  let font' = font sansSerif fontSize mempty

      chrText :: ChrId -> Drawing
      chrText chr =
        Drawing.text font' zero zero (fillColor black) (unwrap chr)

      mkLabel :: ChrId -> NormalizedGlyph
      mkLabel chr = { drawing: inj (SProxy :: SProxy "point") $ chrText chr
                    , horPos:  inj (SProxy :: SProxy "point") $ Normalized (0.5)
                    , verPos: Normalized (0.03) }

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




type VScaleRow r = ( min :: Number
                   , max :: Number
                   , sig :: Number
                   | r )

type VScale = { color :: Color
              , hPad :: Number
              , numSteps :: Int
              | VScaleRow () }

defaultVScaleConfig :: Record (VScaleRow ())
                    -> VScale
defaultVScaleConfig =
  build (insert (SProxy :: SProxy "color")    black
     >>> insert (SProxy :: SProxy "hPad")     0.125
     >>> insert (SProxy :: SProxy "numSteps") 3)


-- type Config (n :: Symbol) a r = forall m. MonadReader ({ n :: a | r}) m


defaultVScaleConfig' :: forall m.
                        MonadReader (Record (VScaleRow ())) m
                     => m VScale
defaultVScaleConfig' = do
  a <- ask
  pure $ build (insert (SProxy :: SProxy "color")    black
     >>> insert (SProxy :: SProxy "hPad")     0.125
     >>> insert (SProxy :: SProxy "numSteps") 3) a



drawVScaleInSlot :: VScale
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

      barOutline = outlineColor vscale.color <> lineWidth 2.0

      vBar = Drawing.path [{x, y: 0.0}, {x, y: size.height}]

      hBar w y = Drawing.path [{x:x-w, y}, {x, y}]
      bars = vBar
          <> foldMap (\i -> hBar 8.0 (i * size.height)) spokes

      unitLabel =
        Drawing.translate (size.width * 0.4 - hPad) (size.height * 0.72)
        $ Drawing.rotate (- Math.pi / 2.0)
        $ Drawing.text (font sansSerif 18 mempty)
            0.0 0.0 (fillColor black) "-log10 (P value)"

      label yN = Drawing.text
                    (font sansSerif 14 mempty)
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
  { entries :: Array LegendEntry
  , hPad :: Number, vPad :: Number
  , fontSize :: Int | r }

defaultLegendConfig :: Array LegendEntry -> LegendConfig ()
defaultLegendConfig entries =
  { entries
  , hPad: 0.2, vPad: 0.2
  , fontSize: 12 }



drawLegendInSlot :: ∀ r.
                    LegendConfig r
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


type RenderedTrack a = { features :: Array a
                       , drawings :: Array DrawingN
                       , labels   :: Array Label
                       , overlaps :: Number -> Point -> Array a }


type Renderer a =
     Canvas.Dimensions
  -> Map ChrId (Array a)
  -> Map ChrId (Pair Number)
  -> RenderedTrack a

type Renderer' a = Map ChrId (Array a)
                -> Map ChrId (Pair Number)
                -> Canvas.Dimensions
                -> List LayerRenderable


pixelSegments :: ∀ r.
                 { segmentPadding :: Number | r }
              -> CoordSys _ _
              -> BrowserCanvas
              -> CoordSysView
              -> Map _ _
pixelSegments conf cSys canvas csView =
  let trackDim = canvas ^. _Track <<< _Dimensions
      -- segmentPadding = 12.0 -- TODO this should be configurable
  in aroundPair (-conf.segmentPadding)
       <$> scaledSegments cSys (viewScale trackDim csView)



pixelSegments' :: ∀ r.
                 { segmentPadding :: Number | r }
              -> CoordSys _ _
              -> Canvas.Dimensions
              -> CoordSysView
              -> Map _ _
pixelSegments' conf cSys trackDim csView =
  aroundPair (-conf.segmentPadding)
       <$> scaledSegments cSys (viewScale trackDim csView)


renderTrack :: ∀ a b.
               _
            -> CoordSys ChrId _
            -> Renderer a
            -> Map ChrId (Array a)
            -> BrowserCanvas
            -> CoordSysView
            -> RenderedTrack a
renderTrack conf cSys renderer trackData canvas =
  let trackDim = canvas ^. _Track <<< _Dimensions
  in renderer trackDim trackData <<< pixelSegments conf cSys canvas


renderTrack' :: ∀ b a r.
                { segmentPadding :: Number | r }
             -> CoordSys ChrId BigInt
             -> Component (b -> Renderer' a)
             -> Map ChrId (Array a)
             -> Layer ( { config :: b, view :: CoordSysView } -> Canvas.Dimensions -> List LayerRenderable)
renderTrack' conf cSys com trackData =
  let
      segs :: Canvas.Dimensions -> CoordSysView -> Map ChrId (Pair Number)
      segs = pixelSegments' conf cSys
  in case com of
        Full     r -> Layer Scrolling NoMask
                      $ Full     $ \c d -> r c.config trackData (segs d c.view) d
        Padded p r -> Layer Scrolling NoMask
                      $ Padded p $ \c d -> r c.config trackData (segs d c.view) d
        _ -> unsafeCrashWith "renderTrack' does not support UI slots yet"


renderTrackLive :: forall r r' l m a b.
                   RowCons l a r r'
                => IsSymbol l
                => MonadReader {|r'} m
                => CoordSys ChrId _
                -> SProxy l
                -> (a -> Renderer b)
                -> a
                -> Map ChrId (Array b)
                -> Eff _ (Cached a
                             (BrowserCanvas
                             -> CoordSysView
                             -> RenderedTrack b))
renderTrackLive cSys _ renderer init trackData = do
  let f :: a -> BrowserCanvas -> CoordSysView -> RenderedTrack b
      f a bc v = let trackDim = bc ^. _Track <<< _Dimensions
                 in renderer a trackDim trackData
                    $ pixelSegments {segmentPadding: 12.0} cSys bc v
  cache f init


renderTrackLive' :: forall r0 r1 l m a b.
                    RowCons l a r0 r1
                 => RowLacks l r0
                 => IsSymbol l
                 => MonadReader { segmentPadding :: Number | r1 } m
                 => CoordSys ChrId _
                 -> SProxy l
                 -> (a -> Renderer b)
                 -> Map ChrId (Array b)
                 -> m (Eff _ (Cached a
                         (BrowserCanvas
                         -> CoordSysView
                         -> RenderedTrack b)))
renderTrackLive' cSys _ renderer trackData = do

  conf <- ask

  let f :: a -> BrowserCanvas -> CoordSysView -> RenderedTrack b
      f a bc v = let trackDim = bc ^. _Track <<< _Dimensions
                 in renderer a trackDim trackData
                    $ pixelSegments conf cSys bc v

      x :: a
      x = Record.unsafeGet (reflectSymbol (SProxy :: SProxy l)) conf

  pure $ cache f x


renderFixedUI :: Component (Canvas.Dimensions -> Drawing)
              -> RenderableLayer Unit
renderFixedUI com = Layer Fixed NoMask $ map f com
  where f draw = \_ d -> pure $ inj _static (draw d)


renderRelativeUI :: CoordSys ChrId BigInt
                 -> Map ChrId (Array NormalizedGlyph)
                 -> BrowserCanvas
                 -> CoordSysView
                 -> Drawing
renderRelativeUI cSys uiGlyphs canvas =
  let
      trackDim = canvas ^. _Track <<< _Dimensions
      overlayDim = canvas ^. _Dimensions

      renderUIElement :: Map ChrId (Array NormalizedGlyph)
                      -> ChrId -> Pair Number -> Array SingleGlyph
      renderUIElement m k s
          = fold $ rescaleNormSingleGlyphs overlayDim.height s
                <$> (Map.lookup k m)

      drawTrackUI :: Pair BigInt -> (ChrId -> Pair Number -> (Array _)) -> Drawing
      drawTrackUI v = foldMap f <<< withPixelSegments cSys trackDim v
        where f {drawing, point} = Drawing.translate point.x point.y (drawing unit)

  in \v -> drawTrackUI (unwrap v) $ renderUIElement uiGlyphs


chrLabelsUI :: ∀ r.
               CoordSys ChrId BigInt
            -> { fontSize :: Int | r }
            -> BrowserCanvas
            -> CoordSysView
            -> Drawing
chrLabelsUI cSys c = renderRelativeUI cSys (chrLabelTrack c cSys)


type Peak x y r = { covers :: Pair x
                  , y :: y
                  , elements :: Array r }
