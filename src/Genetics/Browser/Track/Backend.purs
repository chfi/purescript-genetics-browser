module Genetics.Browser.Track.Backend where

import Color.Scheme.Clrs
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
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array ((:))
import Data.Array as Array
import Data.Distributive (distribute)
import Data.Either (Either, Either(..))
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (class Foldable, fold, foldMap, foldl, for_, length, maximum, minimum, sum)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.List (List(..), mapMaybe)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala, alaF, over, under, unwrap, wrap)
import Data.NonEmpty (foldl1, fromNonEmpty)
import Data.NonEmpty as NE
import Data.Ord.Max (Max(..))
import Data.Ordering (Ordering(..))
import Data.Record as Record
import Data.Record.Unsafe (unsafeSet)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, scanl, traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Unfoldable (singleton, unfoldr)
import Data.Variant (Variant)
import Data.Variant as Variant
import Debug.Trace (trace)
import Debug.Trace as Debug
import Genetics.Browser.DataSource (DataSource)
import Genetics.Browser.Types (Bp(..), BpPerPixel(..), Chr, ChrId(..), Point, Pos, Range, _ChrId, bpToPixels)
import Genetics.Browser.UI.Native (getScreenSize)
import Genetics.Browser.UI.Native.View as View
import Genetics.Browser.View (View, Pixels)
import Global (readFloat, readInt)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getCanvasHeight, getContext2D, setCanvasWidth)
import Graphics.Canvas as C
import Graphics.Drawing (Drawing, circle, fillColor, filled, outlineColor, outlined, rectangle, translate)
import Graphics.Drawing as Drawing
import Math as Math
import Network.HTTP.Affjax as Affjax
import Partial.Unsafe (unsafePartial)
import Text.Parsing.CSV as CSV
import Text.Parsing.Parser as CSV
import Type.Prelude (class IsSymbol, class RowLacks)
import Unsafe.Coerce (unsafeCoerce)

type Frame a = { padding :: Pixels, width :: Pixels, contents :: a }

type ChrCtx r = Map ChrId (Record r)

type ChrFrames r a = ChrCtx (frame :: Frame a | r)

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


-- If both renderers work on exactly the same data point,
-- there's no canonical way of picking between the results,
-- so we just pick the first
catRenderers :: forall a.
                PureRenderer a
             -> PureRenderer a
             -> PureRenderer a
catRenderers r1 r2 = \x -> case r1 x, r2 x of
  Nothing, Nothing -> Nothing
  Just y, _ -> pure y
  _, Just y -> pure y




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
        let r = 2.2
            x = unwrap $ f.pos / size
            y = 1.0 - ((f.score - min) / (max - min))
            c = circle x y r
            out = outlined (outlineColor color) c
            fill = filled (fillColor color) c
            drawing = out <> fill

        point <- normPoint {x, y}
        pure { drawing, point }

  pure renderer



-- A single Frame that contains all data that is required to draw it
type ReadyFrame a = { width :: Pixels
                    , padding :: Pixels
                    , height :: Pixels
                    , contents :: a }


drawFrame :: forall a.
             PureRenderer a
          -> ReadyFrame (Array a)
          -> Drawing
drawFrame r {width, padding, height, contents} = foldMap draw' contents
  where draw' a = case r a of
          Nothing -> mempty
          Just {drawing, point} ->
            let {x,y} = nPointToFrame (width - padding * 2.0) height $ point
            in translate (x + padding) y drawing


-- Draw a collection of ReadyFrames, lined up one after another
drawFrames :: forall a.
              Array (ReadyFrame (Array a))
           -> PureRenderer a
           -> Drawing
drawFrames frames r = fold drawings
  where os = Array.cons 0.0 (framesOffsets frames) -- add 0.0 since scanl in framesOffsets doesn't include seed
        drawings = Array.zipWith (\o -> translate o 0.0) os
                     $ map (drawFrame r) frames


framesOffsets :: forall f r.
                 Traversable f
              => f { width :: Pixels | r }
              -> f Pixels
framesOffsets fs = scanl (\o {width} -> width + o) 0.0 fs


insertFrame :: forall r a.
               RowLacks "frame" r
            => a
            -> { | r }
            -> { frame :: a | r }
insertFrame = Record.insert (SProxy :: SProxy "frame")


createFrames :: forall r a.
                RowLacks "frame" (size :: Bp | r)
             => Pixels
             -> Pixels
             -> Map ChrId {size :: Bp | r}
             -> Map ChrId a
             -> Map ChrId { size :: Bp, frame :: Frame a | r }
createFrames totalWidth padding sizes as = zipMapsWith zippy sizes as
  where ts = totalSize sizes
        mkWidth size = (unwrap $ size / ts) * totalWidth
        mkFrame' {size} a = { padding, width: mkWidth size, contents: a }
        zippy r a = insertFrame (mkFrame' r a) r

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

groupToChrs :: forall r.
               List { chrId :: ChrId | r }
            -> Map ChrId (List { chrId :: ChrId | r })
groupToChrs = foldl (\chrs r@{chrId} -> Map.alter (add r) chrId chrs ) mempty
  where add x Nothing   = Just $ singleton x
        add x (Just xs) = Just $ Cons x xs



chrFrames :: forall a.
             Pixels
          -> Pixels
          -> Map ChrId a
          -> Map ChrId { size :: Bp
                       , frame :: { padding :: Pixels
                                  , width :: Pixels
                                  , contents :: a
                                  }
                       }
chrFrames w p as = createFrames w p chrCtx' as
  where chrCtx' = Map.filterKeys (\k -> k `Map.member` as) mouseChrCtx


addHeight :: forall a r.
             Pixels
          -> Map ChrId { size :: Bp
                       , frame :: { padding :: Pixels
                                  , width :: Pixels
                                  , contents :: a
                                  }
                       | r
                       }
          -> Map ChrId { size :: Bp
                       , frame :: (ReadyFrame a)
                       | r }
addHeight h fs = map f fs
  where f entry = entry { frame = Record.insert (SProxy :: SProxy "height") h entry.frame }


fetchGemmaJSON :: String
               -> Aff _ (Array (GWASFeature ()))
fetchGemmaJSON url = do
  json <- _.response <$> Affjax.get url

  let f :: Json -> Maybe {score :: Number, pos :: Bp, chrId :: ChrId }
      f j = do
            obj <- j ^? _Object
            pos <- Bp <$> obj ^? ix "min" <<< _Number
            pValue <- readFloat <$> obj ^? ix "pValue" <<< _String
            -- chrId <- ChrId <$> obj ^? ix "segment" <<< _String
            let score = (-1.0) * (Math.log pValue / Math.log 10.0)
            let chrId = ChrId "11"
            pure {pos, score, chrId}

  case traverse f =<< json ^? _Array of
    Nothing -> throwError $ error "Failed to parse JSON features"
    Just fs -> pure $ fs


fetchGemmaCSV :: String
              -> Aff _ (List (GWASFeature ()))
fetchGemmaCSV url = do
  csv <- _.response <$> Affjax.get url

  let parsed :: _
      parsed = CSV.runParser csv CSV.defaultParsers.fileHeaded
      parseEntry :: Map String String -> Maybe _
      parseEntry m = do
        chrId <- ChrId <$> Map.lookup "chr" m
        pos <- Bp <<< readFloat <$> Map.lookup "ps" m
        score <- readFloat <$> Map.lookup "af" m
        pure { chrId, pos, score }


  case parsed of
    Left err -> throwError $ error "error parsing csv"
    Right p  -> pure $ mapMaybe parseEntry p



findRenderer :: forall a.
                Map String (PureRenderer a)
             -> PureRenderer (Tuple String a)
findRenderer rs (Tuple s a) = do
  renderer <- Map.lookup s rs
  renderer a



drawMulti :: forall f a.
             Foldable f
          => { w :: Pixels, h :: Pixels, p :: Pixels, y :: Pixels }
          -> Array ChrId
          -> Map String (PureRenderer a)
          -> Map ChrId (Tuple String (f a))
          -> Array ChrId
          -> Drawing
drawMulti {w,h,p,y} allChrs renderers dat chrs =
  let features :: Map ChrId (Tuple String (Array a))
      features = (map <<< map) Array.fromFoldable
                 $ Map.filterKeys (\k -> k `Array.elem` chrs) dat

      readyFrames :: Map ChrId { size :: Bp, frame :: ReadyFrame (Tuple String (Array a)) }
      -- readyFrames :: _
      readyFrames = addHeight h $ chrFrames w p features

      -- needed since the Ord instance on ChrId isn't necessarily correct
      comp (Tuple x _) (Tuple y _ ) = compareChrId allChrs x y

      toDraw :: Array (ReadyFrame (Tuple String (Array a)))
      toDraw = _.frame <<< snd <$> (Array.sortBy comp $ Map.toUnfoldable readyFrames)

      -- r :: PureRenderer (Tuple String a)
      -- r = findRenderer renderers
      r :: PureRenderer (Tuple String a)
      r (Tuple s a) =  do
            renderer <- Map.lookup s renderers
            renderer a

  -- TODO fix this; (Tuple String (Array a)) -> Array (Tuple String a)
  -- in unsafeCoerce unit
  in drawFrames toDraw $ findRenderer renderers




drawGeneric :: forall f a.
               Foldable f
            => { w :: Pixels, h :: Pixels, p :: Pixels, y :: Pixels }
            -> Array ChrId
            -> PureRenderer a
            -> Map ChrId (f a)
            -> Array ChrId
            -> Drawing
drawGeneric {w,h,p,y} allChrs renderer dat chrs =
  let features :: Map ChrId (Array a)
      features = map Array.fromFoldable
                 $ Map.filterKeys (\k -> k `Array.elem` chrs) dat

      readyFrames :: Map ChrId { size :: Bp, frame :: ReadyFrame (Array a) }
      readyFrames = addHeight h $ chrFrames w p features

      -- needed since the Ord instance on ChrId isn't necessarily correct
      comp (Tuple x _) (Tuple y _ ) = compareChrId allChrs x y

      toDraw :: Array (ReadyFrame (Array a))
      toDraw = _.frame <<< snd <$> (Array.sortBy comp $ Map.toUnfoldable readyFrames)

  in drawFrames toDraw renderer





drawGemma :: Pixels -> Pixels -> Pixels -> Pixels
          -> { min :: Number, max :: Number }
          -> Array ChrId
          -> List (GWASFeature ())
          -> Drawing
drawGemma w h p y s chrs gemma =
  let features :: Map ChrId (Array (GWASFeature ()))
      features = map Array.fromFoldable
                 $ Map.filterKeys (\k -> k `Array.elem` chrs)
                 $ groupToChrs gemma

      readyFrames :: Map ChrId { size :: Bp, frame :: ReadyFrame (Array (GWASFeature ())) }
      readyFrames = addHeight h $ chrFrames w p features

      comp (Tuple x _) (Tuple y _ ) = compareChrId mouseChrIds x y
      toDraw :: Array (ReadyFrame (Array (GWASFeature ())))
      toDraw = _.frame <<< snd <$> (Array.sortBy comp $ Map.toUnfoldable readyFrames)

      colors' = zipMapsWith (\r {color} -> Record.insert (SProxy :: SProxy "color") color r)
                  readyFrames mouseColors

      renderer' :: PureRenderer (GWASFeature ())
      renderer' = mkGwasRenderer s colors'

  -- pure $ translate 0.0 (-y) $ drawFrames toDraw renderer'
  in drawFrames toDraw renderer'



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

compareChrId :: Array ChrId -> (ChrId -> ChrId -> Ordering)
compareChrId chrs x y = fromMaybe EQ do
  x <- Array.elemIndex x chrs
  y <- Array.elemIndex y chrs
  pure $ x `compare` y

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

mouseColors :: _
mouseColors = Map.fromFoldable $ Array.zip mouseChrIds $ map (\x -> {color: x})
              [ navy, blue, aqua, teal, olive
              , green, lime, yellow, orange, red
              , maroon, fuchsia, purple, navy, blue
              , aqua, teal, olive, green, lime
              , yellow ]


mouseChrCtx :: ChrCtx (size :: Bp)
mouseChrCtx = map (\size -> { size }) mouseChrs


mouseChrSize :: Bp -> ChrId -> Bp
mouseChrSize def chr = fromMaybe def $ Map.lookup chr mouseChrs
