module Genetics.Browser.TruTrack where

import Prelude

import Color (Color)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (class MonadReader, Reader, ReaderT(..), ask, runReader, runReaderT)
import Control.MonadPlus (guard)
import Data.Array ((:))
import Data.Array as Array
import Data.Distributive (distribute)
import Data.Either (Either, Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, fold, foldMap, foldl, for_, length, maximum, minimum, sum)
import Data.List (List(..), mapMaybe)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala, alaF, unwrap)
import Data.NonEmpty (foldl1, fromNonEmpty)
import Data.NonEmpty as NE
import Data.Ord.Max (Max(..))
import Data.Record as Record
import Data.Record.Unsafe (unsafeSet)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, scanl, traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Unfoldable (unfoldr)
import Debug.Trace (trace)
import Genetics.Browser.DataSource (DataSource)
import Genetics.Browser.Types (Bp(..), BpPerPixel(..), Chr, ChrId(..), Point, Range, Pos, bpToPixels)
import Genetics.Browser.UI.Native (getScreenSize)
import Genetics.Browser.UI.Native.View as View
import Genetics.Browser.View (View, Pixels)
import Global (readFloat, readInt)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getCanvasHeight, getContext2D, setCanvasWidth)
import Graphics.Canvas as C
import Graphics.Drawing (Drawing, circle, fillColor, filled, outlineColor, outlined, translate)
import Graphics.Drawing as Drawing
import Math as Math
import Network.HTTP.Affjax as Affjax
import Partial.Unsafe (unsafePartial)
import Text.Parsing.CSV as CSV
import Text.Parsing.Parser as CSV
import Type.Prelude (class RowLacks)
import Unsafe.Coerce (unsafeCoerce)

type Frame a = { padding :: Pixels, width :: Pixels, contents :: a }

type ChrCtx r = Map ChrId (Record r)

type ChrFrames r a = ChrCtx (frame :: Frame a | r)

chrIdsEq :: forall a b.
            Map ChrId a
         -> Map ChrId b
         -> Boolean
chrIdsEq a b = (Map.keys a) == (Map.keys b)

chrIdsSub :: forall a b.
             Map ChrId a
          -> Map ChrId b
          -> Boolean
chrIdsSub a b = setA `Map.isSubmap` setB
  where setA = map (const unit) a
        setB = map (const unit) b


totalSize :: forall r.
             ChrCtx (size :: Bp | r)
          -> Bp
totalSize chrCtx = alaF Additive foldMap _.size chrCtx


newtype Normalized a = Normalized a

normalizePoint :: { width :: Number
                  , height :: Number }
               -> Point
               -> Normalized Point
normalizePoint {width, height} {x, y} = Normalized { x: x', y: y' }
  where x' = x / width
        y' = y / height

normPoint :: Point -> Maybe (Normalized Point)
normPoint p@{x, y}
  | x < 0.0 || x > 1.0 = Nothing
  | y < 0.0 || y > 1.0 = Nothing
  | otherwise = Just $ Normalized p

nPointToFrame :: Pixels -> Pixels -> Normalized Point -> Point
nPointToFrame w h (Normalized p) = {x: p.x * w, y: p.y * h}


-- A renderer draws a single value to a normalized "canvas",
-- i.e. the whole chromosome is located in x = (0.0, 1.0),
-- and the whole track height in y = (0.0, 1.0).
-- It can fail if the given feature for some reason cannot be rendered,
-- either because the feature lacks information, or because the context lacks something.
type PureRenderer a = a -> Maybe { drawing :: Drawing
                                 , point   :: Normalized Point
                                 }


type GWASFeature r = { score :: Number
                     , pos :: Bp
                     , chrId :: ChrId | r }

mkGwasRenderer :: forall m rf rctx.
                  MonadReader (ChrCtx (size :: Bp, color :: Color | rctx)) m
               -- => MonadError Unit m
               => {min :: Number, max :: Number}
               -> m (PureRenderer (GWASFeature rf))
mkGwasRenderer {min, max} = do
  ctx <- ask
  let renderer f = do
        size <- _.size <$> Map.lookup f.chrId ctx
        color <- _.color <$> Map.lookup f.chrId ctx

        let r = 1.0
            x = (unwrap $ f.pos / size) - r
            y = (f.score - min / max - min) - r
            c = circle x y r
            out = outlined (outlineColor color) c
            fill = filled (fillColor color) c
            drawing = out <> fill
        point <- normPoint {x, y}
        pure { drawing, point }

  pure renderer

                  , point :: a -> Normalized Point
                  }


drawFrame :: forall a r.
             { width :: Pixels
             , padding :: Pixels
             , height :: Pixels
             , frame :: Frame (Array a) | r}
          -> Renderer a
          -> Drawing
drawFrame {width, padding, height, frame} {draw, point} = foldMap draw' frame.contents
  where draw' a = let {x,y} = nPointToFrame (width - padding * 2.0) height $ point a
                      drawing = draw a
                  in translate (x + padding) y drawing



offsets :: forall f r.
           Traversable f
        => f { width :: Pixels | r }
        -> f Pixels
offsets fs = scanl (\o {width} -> width + o) 0.0 fs


addOffsets :: forall k r.
              Ord k
           => Map k { width :: Pixels | r }
           -> Map k { width :: Pixels, offset :: Pixels | r }
addOffsets fs = zipMapsWith (unsafeSet "offset") os fs
  where os = offsets fs



-- We simply throw away existing width, offset, height data if it exists
fitFrames :: forall f a r.
             Foldable f
          => Pixels
          -> f { size :: Bp | r }
          -> f { width :: Pixels
               , offset :: Pixels | r }
fitFrames totalWidth fs = unsafeCoerce unit
  where totalSize = alaF Additive foldMap _.size fs
        w size = unwrap $ size / totalSize



-- TODO reasonable way of defining frame/track y-position, based on:

-- given a position relative to the track where 0.0 = bottom, 1.0 = top,
-- information about where the track will be drawn and how,
-- return the canvas Y-coordinate
type YPosInfo = { offset :: Pixels, height :: Pixels }

posPixelsY :: YPosInfo
           -> Number
           -> Pixels
posPixelsY { offset, height } rel = height - (rel * height + offset)


-- type Renderer eff a =
--   { hPos :: a -> XPosInfo -> Pixels
--   , vPos :: a -> YPosInfo -> Pixels
--   , render :: a -> Context2D -> Eff eff Unit
--   , hit :: Maybe (a -> { x :: Pixels, y :: Pixels } -> a)
--   }


-- gwasRenderer :: forall a.
--                 {min :: Number, max :: Number}
--              -> (a -> String)
--              -> Renderer _ {score :: Number, pos :: Bp, chr :: a}
-- gwasRenderer {min, max} color = { hPos, vPos, render, hit: Nothing }
--   where hPos a xInfo = posPixelsX xInfo a.pos
--         nScore s = (s - min) / (max - min)
--         vPos a yInfo = posPixelsY yInfo (nScore a.score)
--         render a ctx = renderGlyph ctx $ do
--           stroke (color a.chr)
--           fill (color a.chr)
--           circle { x: 0.0, y: 0.0 } 5.0



insertFrame :: forall r a.
               RowLacks "frame" r
            => a
            -> { | r }
            -> { frame :: a | r }
insertFrame = Record.insert (SProxy :: SProxy "frame")


mkFrame :: forall r a.
            RowLacks "frame" (size :: Bp | r)
         => Pixels
         -> (Bp -> Pixels)
         -> {size :: Bp | r}
         -> a
         -> {size :: Bp, frame :: Frame a | r}
mkFrame padding getWidth chr a = chr'
  where chr' :: {size :: Bp, frame :: Frame a | r}
        chr' = insertFrame frame chr
        frame :: Frame a
        frame = { padding, width: getWidth chr.size, contents: a }


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


chrSubrange :: forall a r.
               Range r
            -> Map ChrId a
            -> Map ChrId a
chrSubrange {lHand, rHand} = Map.submap (Just lHand.chrId) (Just rHand.chrId)



-- renderFrame :: _
-- renderFrame = ?idk


fetchGemma :: String
           -> Aff _ (List { chr :: ChrId, pos :: Bp, score :: Number })
fetchGemma url = do
  liftEff $ log "fetching gwas"
  csv <- _.response <$> Affjax.get url
  let parsed :: _
      parsed = CSV.runParser csv CSV.defaultParsers.fileHeaded
      parseEntry :: Map String String -> Maybe _
      parseEntry m = do
        chr <- ChrId <$> Map.lookup "chr" m
        pos <- Bp <<< readFloat <$> Map.lookup "ps" m
        score <- readFloat <$> Map.lookup "af" m
        pure { chr, pos, score }

  case parsed of
    Left err -> throwError $ error "error parsing csv"
    Right p  -> pure $ mapMaybe parseEntry p


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


mouseChrCtx :: ChrCtx (size :: Bp)
mouseChrCtx = map (\size -> { size }) mouseChrs


mouseChrSize :: Bp -> ChrId -> Bp
mouseChrSize def chr = fromMaybe def $ Map.lookup chr mouseChrs
