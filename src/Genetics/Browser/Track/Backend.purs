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
import Data.Array ((:))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (class Foldable, fold, foldMap, foldl, maximum, sum)
import Data.Int as Int
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, ala, alaF, unwrap)
import Data.Ord.Max (Max(..))
import Data.Ratio (Ratio(..), denominator, numerator, (%))
import Data.Record as Record
import Data.Symbol (SProxy(..))
import Data.Traversable (scanl, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Variant (class Contractable, Variant)
import Data.Variant as Variant
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId), Point)
import Genetics.Browser.View (Pixels)
import Graphics.Drawing (Drawing, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, scale, translate)
import Graphics.Drawing as Drawing
import Graphics.Drawing.Font (font, sansSerif)
import Network.HTTP.Affjax as Affjax
import Type.Prelude (class IsSymbol, class RowLacks)
import Unsafe.Coerce (unsafeCoerce)




-- This type represents a point on the *browser*, not the *genome*.
-- It includes spaces between chromosomes, for example.
newtype BrowserPoint = BrowserPoint (Ratio BigInt)

derive instance eqBrowserPoint :: Eq BrowserPoint
derive instance ordBrowserPoint :: Ord BrowserPoint
derive instance newtypeBrowserPoint :: Newtype BrowserPoint _



newtype CoordSystem = CoordSystem { genLength :: BigInt
                                  , toGenome :: Ratio BigInt -> Maybe (Tuple ChrId Bp)
                                  , toGlobal :: Tuple ChrId Bp -> Maybe (Ratio BigInt)
                                  , frames :: Array (Tuple ChrId
                                                     { start :: Ratio BigInt
                                                     , end :: Ratio BigInt
                                                     })
                                  }

toBigInt :: Bp -> BigInt
toBigInt (Bp x) = BigInt.fromInt $ Int.round $ x


chrIntervals :: Array (Tuple ChrId Bp)
             -> Array (Tuple ChrId (Tuple BigInt BigInt))
chrIntervals chrs = case Array.uncons chrs of
  Nothing -> []
  Just {head, tail} ->
    let first = Tuple (toBigInt (Bp 0.0)) (toBigInt (snd head))
        g :: Tuple BigInt BigInt -> Bp -> Tuple BigInt BigInt
        g (Tuple start end) size = (Tuple end (end+(toBigInt size)))
        (Tuple ids sizes) = Array.unzip chrs
    in Array.zip ids (Array.cons first $ scanl g first sizes)


toBp :: Ratio BigInt -> Ratio BigInt -> Ratio BigInt -> Bp -> Bp
toBp start end pos bp =
  let norm = (pos - start) - (end - start)
      norm' = (BigInt.toNumber $ numerator norm) / (BigInt.toNumber $ denominator norm)
  in (Bp norm') * bp


frameToGenome :: Array (Tuple ChrId { start :: Ratio BigInt
                                    , end :: Ratio BigInt })
         -> Ratio BigInt
         -> Maybe (Tuple ChrId Bp)
frameToGenome frames r = do
  chr <- Array.find (\(Tuple _ {start, end}) -> start <= r && r <= end) frames

  let chr' :: _
      chr' = chr

  pure $ unsafeCoerce unit


-- toGlobal :: Tuple ChrId Bp -> Maybe (Ratio BigInt)
-- toGlobal = unsafeCoerce unit


mkCoordSystem :: forall f.
                 Functor f
              => Foldable f
              => f (Tuple ChrId Bp)
              -> CoordSystem
mkCoordSystem chrs = CoordSystem { genLength, toGenome, toGlobal, frames }
  where genLength :: BigInt
        genLength = Additive `ala` foldMap $ map (toBigInt <<< snd) chrs

        frames :: Array (Tuple ChrId {start :: Ratio BigInt, end :: Ratio BigInt})
        frames = map (\(Tuple s e) -> {start: s % genLength, end: e % genLength})
                   <$> (chrIntervals $ Array.fromFoldable chrs)

        toGenome :: Ratio BigInt -> Maybe (Tuple ChrId Bp)
        toGenome r = unsafeCoerce unit

        toGlobal :: Tuple ChrId Bp -> Maybe (Ratio BigInt)
        toGlobal = unsafeCoerce unit


-- testin :: List _ -> _
-- testin = Additive `ala` foldMap

-- To map a BrowserPoint to a point on the genome, we need to know
-- the intervals of the chromosomes, and their respective sizes.
-- I.e. we need to "invert" the Frames.
-- To simplify that we should simplify Frames, decoupling the concept of width from height.
-- Then produce a function
{-
  x in [0.0, 1.0] -> Tuple ChrId Bp
-}
-- So extract the `frames` function from `drawData'` (again).
-- Or, well
-- the drawData' function should grow to handle events on glyphs,
-- which will involve this type of thing as well.

-- toGenome :: BrowserPoint -> ChrCtx



type Frame a = { padding :: Pixels, width :: Pixels, contents :: a }

type ChrCtx r = Map ChrId (Record r)

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


-- Drawings are not entirely pure. They can depend on the width of the
-- frame, if they something that spans across a range of the genome.
-- So, the `drawing` field should really have type
--   Pixels -> Drawing
-- where the `Pixels` is the width of the frame. Renderers already
-- have access to the chr size when created, so the width is all that's needed.
type PureRenderer' a = a -> Maybe { drawing :: Pixels -> Drawing
                                  , point   :: Normalized Point
                                  }


tagRenderer :: forall a sym r1 r2.
               RowCons sym a r1 r2
            => IsSymbol sym
            => SProxy sym
            -> PureRenderer a
            -> PureRenderer (Variant r2)
tagRenderer sym renderer var = Variant.prj sym var >>= renderer


combineRenderers :: forall r1 r2 r3.
                    Union r1 r2 r3
                 => Contractable r3 r1
                 => Contractable r3 r2
                 => PureRenderer (Variant r1)
                 -> PureRenderer (Variant r2)
                 -> PureRenderer (Variant r3)
combineRenderers r1 r2 var = (Variant.contract var >>= r1) <|>
                             (Variant.contract var >>= r2)


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


pureRender :: forall f a.
              Filterable f
           => PureRenderer a
           -> f a
           -> f { drawing :: Drawing, point :: Normalized Point }
pureRender r as = filterMap r as


-- TODO simplify these two functions by extracting `padding` and `height` to another argument;
-- after all, only `width` changes per frame
drawPureFrame :: forall f r.
                 Foldable f
              => f { drawing :: Drawing, point :: Normalized Point | r }
              -> { width :: Pixels, padding :: Pixels, height :: Pixels }
              -> Drawing
drawPureFrame fs { width, padding, height } = foldMap render' fs
  where render' {drawing, point} =
            let {x,y} = nPointToFrame (width - padding * 2.0) height $ point
            in translate (x + padding) y drawing


-- Draw a collection of , lined up one after another
drawPureFrames :: forall a.
                  Array (Tuple (Array { drawing :: Drawing, point :: Normalized Point })
                               { width :: Pixels, padding :: Pixels, height :: Pixels })
               -> Drawing
drawPureFrames frames = fold $ Array.zipWith (\o -> translate o 0.0) os drawings'
  where os = framesOffsets $ snd <$> frames
        drawings' = map (uncurry drawPureFrame) frames


framesOffsets :: forall r.
                 Array { width :: Pixels | r }
              -> Array Pixels
framesOffsets fs = 0.0 : scanl (\o {width} -> width + o) 0.0 fs
    -- add 0.0 since scanl in framesOffsets doesn't include seed


shiftRendererMinY :: forall rf.
                      RowLacks "minY" rf
                   => Number
                   -> { min :: Number, max :: Number }
                   -> PureRenderer (Record rf)
                   -> PureRenderer (Record (minY :: Number | rf))
shiftRendererMinY dist s render f = do
  {drawing, point} <- render (Record.delete (SProxy :: SProxy "minY") f)
  let {x,y} = unwrap point
      normY = (f.minY - s.min) / (s.max - s.min)
      y' = min (normY + dist) 0.95

  point' <- normPoint {x, y: y'}
  pure {drawing, point: point'}


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


groupToChrs :: forall a f rData.
               Monoid (f {chrId :: ChrId | rData})
            => Foldable f
            => Applicative f
            => f { chrId :: ChrId | rData }
            -> Map ChrId (f { chrId :: ChrId | rData })
groupToChrs = foldl (\chrs r@{chrId} -> Map.alter (add r) chrId chrs ) mempty
  where add x Nothing   = Just $ pure x
        add x (Just xs) = Just $ pure x <> xs


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


fetchGemmaJSON :: String -> Aff _ (Array (GWASFeature ()))
fetchGemmaJSON = fetchJSON gemmaJSONParse


type GeneRow r = ( geneID :: String
                 , desc :: String
                 , start :: Bp
                 , end :: Bp
                 , name :: String
                 , chrId :: ChrId | r )

type Gene r = Record (GeneRow r)



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


fetchGeneJSON :: String -> Aff _ (Array (Gene ()))
fetchGeneJSON = fetchJSON geneJSONParse


type AnnotRow r = ( geneID :: String
                  , desc :: String
                  , pos :: Bp
                  , name :: String
                  , chrId :: ChrId | r )

type Annot r = Record (AnnotRow r)


mkAnnotRenderer :: forall m rf rctx.
                   MonadReader (ChrCtx (size :: Bp | rctx)) m
                => m (PureRenderer (Annot rf))
mkAnnotRenderer = do
  ctx <- ask
  let font' = font sansSerif 12 mempty
  let renderer f = do
        size <- _.size <$> Map.lookup f.chrId ctx

        let rad = 5.0
            x = unwrap $ f.pos / size
            y = 0.0
            c = circle x y rad
            out  = outlined (outlineColor maroon <> lineWidth 3.0) c
            fill = filled   (fillColor red) c
            -- the canvas is flipped so y-axis increases upward, need to flip the text too
            text' = scale 1.0 (-1.0)
                    $ Drawing.text font' (x+7.5) (y+2.5) (fillColor black) f.name
            drawing = out <> fill <> text'

        point <- normPoint {x, y}
        pure { drawing, point }

  pure renderer




fetchAnnotJSON :: String -> Aff _ (Array (Annot ()))
fetchAnnotJSON str = map toAnnot <$> fetchJSON geneJSONParse str
  where toAnnot :: Gene () -> Annot ()
        toAnnot gene = { geneID: gene.geneID
                       , desc: gene.desc
                       , pos: gene.start
                       , name: gene.name
                       , chrId: gene.chrId }



bumpAnnots :: forall rAnnot rGWAS.
             RowLacks "minY" (AnnotRow rAnnot)
          => Bp
          -> Map ChrId (List (GWASFeature rGWAS))
          -> Map ChrId (List (Annot rAnnot))
          -> Map ChrId (List (Annot (minY :: Number | rAnnot)))
bumpAnnots radius = zipMapsWith (map <<< bumpAnnot)
  where maxScoreIn :: Bp -> Bp -> List (GWASFeature rGWAS) -> Number
        maxScoreIn l r gwas = fromMaybe 0.5 $ maximum
                              $ map (\g -> if g.pos >= l && g.pos <= r
                                           then g.score else 0.0) gwas
        bumpAnnot :: List (GWASFeature rGWAS) -> Annot rAnnot -> Annot (minY :: Number | rAnnot)
        bumpAnnot l g = let minY = maxScoreIn (g.pos - radius) (g.pos + radius) l
                       in Record.insert (SProxy :: SProxy "minY") minY g


getDataDemo :: { gwas :: String
               , annots :: String }
            -> Aff _ { gwas  :: Map ChrId (List (GWASFeature ()       ))
                     , annots :: Map ChrId (List (Annot (minY :: Number)))
                     }
getDataDemo urls = do
  gwas <- fetchGemmaJSON urls.gwas
  annots' <- fetchAnnotJSON urls.annots
  -- Divide data by chromosomes
  let gwasChr :: Map ChrId (List (GWASFeature ()))
      gwasChr = List.fromFoldable <$> groupToChrs gwas
  let annotsChr :: Map ChrId (List (Annot ()))
      annotsChr = List.fromFoldable <$> groupToChrs annots'
  let bumpedAnnots :: Map ChrId (List (Annot (minY :: Number)))
      bumpedAnnots = bumpAnnots (Bp 1000000.0) gwasChr annotsChr

  pure { gwas: gwasChr, annots: bumpedAnnots }



renderersDemo :: { min :: Number, max :: Number }
              -> { gwas  :: PureRenderer (GWASFeature ()       )
                 , annots :: PureRenderer (Annot (minY :: Number)) }
renderersDemo s = { gwas, annots }
  where colorsCtx = zipMapsWith (\r {color} -> Record.insert (SProxy :: SProxy "color") color r)
                      mouseChrCtx mouseColors
        gwas :: PureRenderer (GWASFeature ())
        gwas = mkGwasRenderer s colorsCtx

        annots :: PureRenderer (Annot (minY :: Number))
        annots = shiftRendererMinY 0.06 s $ mkAnnotRenderer mouseChrCtx


ruler :: forall r.
         { min :: Number, max :: Number }
      -> Number
      -> Color
      -> { width :: Pixels, height :: Pixels, yOffset :: Pixels | r}
      -> Drawing
ruler {min, max} val color f = outlined outline rulerDrawing
  where normY = (val - min) / (max - min)
        y = f.height - (normY * f.height + f.yOffset)
        outline = outlineColor color
        rulerDrawing = Drawing.path [{x: 0.0, y}, {x: f.width, y}]





drawDemo :: forall f.
            Foldable f
         => Filterable f
         => { min :: Number, max :: Number }
         -> Number
         -> { width :: Pixels, height :: Pixels, padding :: Pixels, yOffset :: Pixels }
         -> { gwas :: Map ChrId (f (GWASFeature ()))
            , annots :: Map ChrId (f (Annot (minY :: Number))) }
         -> Array ChrId
         -> Drawing
drawDemo s sig f {gwas, annots} =
  let renderers = renderersDemo s
      ruler' = ruler s sig red
      drawGwas   = drawData f mouseChrCtx renderers.gwas   gwas
      drawAnnots = drawData f mouseChrCtx renderers.annots annots
  in \chrs -> (drawGwas chrs) <> (drawAnnots chrs) <> ruler' f


-- Draw data by rendering it only once
drawData :: forall f a r.
            Foldable f
         => Filterable f
         -- NOTE! the width is the canvas width, not frame. Same w/ yOffset
         => { width :: Pixels, height :: Pixels, padding :: Pixels, yOffset :: Pixels }
         -> Map ChrId { size :: Bp | r}
         -> PureRenderer a
         -> Map ChrId (f a)
         -> Array ChrId
         -> Drawing
drawData frameBox chrCtx renderer dat =
  let drawings :: Map ChrId (Array {drawing :: Drawing, point :: _})
      drawings = map (Array.fromFoldable <<< pureRender renderer) dat

      mkFrame :: Array ChrId -> ChrId -> Maybe {width :: _, height :: _, padding :: _}
      mkFrame chrs' chr = do
        {size} <- Map.lookup chr chrCtx
        let total = sum $ filterMap (\c -> _.size <$> Map.lookup c chrCtx) chrs'
            width = (unwrap $ size / total) * frameBox.width
        pure { height: frameBox.height - frameBox.yOffset, padding: frameBox.padding, width }

      frames :: Array ChrId -> Array (Tuple (Array _) (_))
      frames chrs' = Array.zip (map (\c -> fromMaybe [] $ Map.lookup c drawings) chrs')
                               (filterMap (mkFrame chrs') chrs')

  in \chrs -> translate 0.0 (frameBox.height - frameBox.yOffset)
                $ scale 1.0 (-1.0)
                $ drawPureFrames (frames chrs)



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
