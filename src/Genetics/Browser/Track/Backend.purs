module Genetics.Browser.Track.Backend where

import Prelude

import Color (Color, black)
import Color.Scheme.Clrs (aqua, blue, fuchsia, green, lime, maroon, navy, olive, orange, purple, red, teal, yellow)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (class MonadReader, ask)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array as Array
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, fold, foldMap, foldl)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, alaF, unwrap)
import Data.Record as Record
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, scanl, traverse)
import Data.Tuple (Tuple(Tuple), snd, uncurry)
import Data.Unfoldable (singleton)
import Data.Variant (class Contractable, Variant)
import Data.Variant as Variant
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId), Point)
import Genetics.Browser.View (Pixels)
import Graphics.Drawing (Drawing, circle, fillColor, filled, outlineColor, outlined, rectangle, scale, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Network.HTTP.Affjax as Affjax
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

derive instance newtypeNormalized :: Newtype (Normalized a) _

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
            y = (f.score - min) / (max - min)
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


gemmaJSONParse :: Json -> Maybe (GWASFeature ())
gemmaJSONParse j = do
  obj <- j ^? _Object
  chrId <- ChrId <$> obj ^? ix "chr" <<< _String
  pos   <- Bp    <$> obj ^? ix "ps"  <<< _Number
  score <-           obj ^? ix "af"  <<< _Number
  pure {pos, score, chrId}

fetchGemmaJSON = fetchJSON gemmaJSONParse


type GeneRow' r = ( geneID :: String
                  , desc :: String
                  , start :: Bp
                  , end :: Bp
                  , name :: String | r )

type Gene' r = Record (GeneRow' r)


type GeneRow r = ( geneID :: String
                 , desc :: String
                 , start :: Bp
                 , end :: Bp
                 , name :: String
                 , chrId :: ChrId | r )

type Gene r = Record (GeneRow r)



gene'JSONParse :: Json -> Maybe (Gene' ())
gene'JSONParse j = do
  obj    <- j ^? _Object
  geneID <- obj ^? ix "Gene stable ID" <<< _String
  desc   <- obj ^? ix "Gene description" <<< _String
  name   <- obj ^? ix "Gene name" <<< _String
  start  <- Bp    <$> obj ^? ix "Gene start (bp)"  <<< _Number
  end    <- Bp    <$> obj ^? ix "Gene end (bp)"  <<< _Number
  pure {geneID, desc, name, start, end}



geneJSONParse :: Json -> Maybe (Gene ())
geneJSONParse j = do
  obj    <- j ^? _Object
  geneID <- obj ^? ix "Gene stable ID" <<< _String
  desc   <- obj ^? ix "Gene description" <<< _String
  name   <- obj ^? ix "Gene name" <<< _String
  start  <- Bp    <$> obj ^? ix "Gene start (bp)"  <<< _Number
  end    <- Bp    <$> obj ^? ix "Gene end (bp)"  <<< _Number
  chrId  <- ChrId <$> obj ^? ix "ChrId" <<< _String
  pure {geneID, desc, name, start, end, chrId}


fetchGene'JSON :: String -> Aff _ (Array (Gene' ()))
fetchGene'JSON = fetchJSON gene'JSONParse


geneFetchChrId :: forall r.
                  RowLacks "chrId" (GeneRow' r)
               => Gene' r
               -> Aff _ (Gene r)
geneFetchChrId gene = do
  let url = "http://rest.ensembl.org/lookup/id/" <>
            gene.geneID <> "?content-type=application/json"

  res <- _.response <$> Affjax.get url

  let chrId :: _
      chrId = do
        obj <- res ^? _Object
        ChrId <$> obj ^? ix "seq_region_name" <<< _String

  case chrId of
    Nothing  -> throwError $ error $ "Could not find chrId for gene " <> gene.geneID
    Just chr -> pure $ Record.insert (SProxy :: SProxy "chrId") chr gene


fetchGeneJSONEnsembl :: String -> Aff _ (Array (Gene ()))
fetchGeneJSONEnsembl url = traverse geneFetchChrId =<< fetchJSON gene'JSONParse url


fetchGeneJSON :: String -> Aff _ (Array (Gene ()))
fetchGeneJSON = fetchJSON geneJSONParse


mkGeneRenderer :: forall m rf rctx.
                  MonadReader (ChrCtx (size :: Bp | rctx)) m
               => m (PureRenderer (Gene rf))
mkGeneRenderer = do
  ctx <- ask

  let font' = font sansSerif 12 mempty
  let renderer f = do
        size <- _.size <$> Map.lookup f.chrId ctx
        let rad = 20.0
            lHand = unwrap $ f.start / size
            rHand = unwrap $ f.end / size
            w = (rHand - lHand) * unwrap size
            x = lHand + (rHand - lHand)
            y = 0.5
            rect = rectangle lHand (y - rad/2.0) w (rad/2.0)
            out = outlined (outlineColor red) rect
            fill = filled (fillColor maroon) rect
            -- the canvas is flipped so y-axis increases upward, need to flip the text too
            text' = scale 1.0 (-1.0)
                    $ Drawing.text font' x (y-0.25) (fillColor black) f.desc
            drawing = out <> fill <> text'

        point <- normPoint {x, y}
        pure { drawing, point }

  pure renderer


shiftRendererScore :: forall rf.
                          RowLacks "score" rf
                       => Number
                       -> { min :: Number, max :: Number }
                       -> PureRenderer (Record rf)
                       -> PureRenderer (Record (score :: Number | rf))
shiftRendererScore dist s render f = do
  {drawing, point} <- render (Record.delete (SProxy :: SProxy "score") f)
  let {x,y} = unwrap point
      y' = min (y + f.score - dist) 1.0
  point' <- normPoint {x, y: y'}
  pure {drawing, point: point'}


findRenderer :: forall a.
                Map String (PureRenderer a)
             -> PureRenderer (Tuple String a)
findRenderer rs (Tuple s a) = do
  renderer <- Map.lookup s rs
  renderer a



{-
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
      -- toDraw = _.frame <<< snd <$> (Array.sortBy comp $ Map.toUnfoldable readyFrames)
      toDraw = _.frame <<< snd <$> ((\chr -> Map.lookup chr readyFrames) <$> chrs

  in drawFrames toDraw renderer
-}


drawGenes :: forall r.
             { w :: Pixels, h :: Pixels, p :: Pixels, y :: Pixels }
          -> { min :: Number, max :: Number }
          -> List (Gene r)
          -> Array ChrId
          -> Drawing
drawGenes {w,h,p,y} s genes chrs =
  let features :: Map ChrId (Array _)
      features = map Array.fromFoldable
                 $ Map.filterKeys (\k -> k `Array.elem` chrs)
                 $ groupToChrs genes

      readyFrames :: Map ChrId { size :: Bp, frame :: ReadyFrame (Array _) }
      readyFrames = addHeight h $ chrFrames w p features

      comp (Tuple x _) (Tuple y _ ) = compareChrId mouseChrIds x y
      toDraw :: Array (ReadyFrame (Array _))
      toDraw = _.frame <<< snd <$> (Array.sortBy comp $ Map.toUnfoldable readyFrames)

      renderer' :: PureRenderer (Gene r)
      renderer' = mkGeneRenderer readyFrames

  in translate 0.0 (h-y) $ scale 1.0 (-1.0) $ drawFrames toDraw renderer'


drawGemma :: forall r.
             { w :: Pixels, h :: Pixels, p :: Pixels, y :: Pixels }
          -> { min :: Number, max :: Number }
          -> Array ChrId
          -> List (GWASFeature r)
          -> Drawing
drawGemma {w,h,p,y} s chrs gemma =
  let features :: Map ChrId (Array (GWASFeature r))
      features = map Array.fromFoldable
                 $ Map.filterKeys (\k -> k `Array.elem` chrs)
                 $ groupToChrs gemma

      readyFrames :: Map ChrId { size :: Bp, frame :: ReadyFrame (Array (GWASFeature r)) }
      readyFrames = addHeight h $ chrFrames w p features

      comp (Tuple x _) (Tuple y _ ) = compareChrId mouseChrIds x y
      toDraw :: Array (ReadyFrame (Array (GWASFeature r)))
      toDraw = _.frame <<< snd <$> (Array.sortBy comp $ Map.toUnfoldable readyFrames)

      colorsCtx = zipMapsWith (\r {color} -> Record.insert (SProxy :: SProxy "color") color r)
                    readyFrames mouseColors

      renderer' :: PureRenderer (GWASFeature r)
      renderer' = mkGwasRenderer s colorsCtx

  in translate 0.0 (h-y) $ scale 1.0 (-1.0) $ drawFrames toDraw renderer'



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


mouseChrSize :: Bp -> ChrId -> Bp
mouseChrSize def chr = fromMaybe def $ Map.lookup chr mouseChrs
