module Genetics.Browser.Track where

import Prelude

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVar, makeVar, takeVar)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (StateT(..))
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (Json, _Array, _Number, _Object, _String)
import Data.Array (foldl, (!!), (..), (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, for_, length)
import Data.Int as Int
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.List (List, mapMaybe)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, Maybe(..), fromJust, fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala, alaF, unwrap, wrap)
import Data.NonEmpty (foldl1, fromNonEmpty)
import Data.NonEmpty as NE
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Debug.Trace (trace)
import Genetics.Browser.DataSource (DataSource)
import Genetics.Browser.Glyph (Glyph, circle, fill, stroke)
import Genetics.Browser.GlyphF.Canvas (renderGlyph)
import Genetics.Browser.Types (Bp(..), BpPerPixel(..), Chr, ChrId(..), Point, Range, bpToPixels)
import Genetics.Browser.UI.Native (getScreenSize)
import Genetics.Browser.UI.Native.View as View
import Genetics.Browser.View (View, Pixels)
import Global (readFloat, readInt)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getCanvasHeight, getContext2D, setCanvasWidth)
import Graphics.Canvas as C
import Math as Math
import Network.HTTP.Affjax as Affjax
import Partial.Unsafe (unsafePartial)
import Text.Parsing.CSV as CSV
import Text.Parsing.Parser as CSV


type Track aff eff a = { source :: DataSource aff a
                       , render :: Array a -> Eff eff Unit
                       , getPoint :: Point -> Array a
                       , chrSize :: Chr -> Bp
                       }



-- given a size on the chromosome,
-- the position of the chromosome,
-- return a function from scale to ncanvas X-coordinate
-- TODO this should be more closely connected to Frames
type XPosInfo = { size :: Bp, scale :: BpPerPixel }

posPixelsX :: XPosInfo
           -> Bp
           -> Pixels
posPixelsX { scale } pos = bpToPixels scale pos



-- given a position relative to the track where 0.0 = bottom, 1.0 = top,
-- information about where the track will be drawn and how,
-- return the canvas Y-coordinate
type YPosInfo = { offset :: Pixels, height :: Pixels }

posPixelsY :: YPosInfo
           -> Number
           -> Pixels
posPixelsY { offset, height } rel = height - (rel * height + offset)



type Renderer eff a =
  { hPos :: a -> XPosInfo -> Pixels
  , vPos :: a -> YPosInfo -> Pixels
  , render :: a -> Context2D -> Eff eff Unit
  , hit :: Maybe (a -> { x :: Pixels, y :: Pixels } -> a)
  }


gwasRenderer :: forall a.
                {min :: Number, max :: Number}
             -> (a -> String)
             -> Renderer _ {score :: Number, pos :: Bp, chr :: a}
gwasRenderer {min, max} color = { hPos, vPos, render, hit: Nothing }
  where hPos a xInfo = posPixelsX xInfo a.pos
        nScore s = (s - min) / (max - min)
        vPos a yInfo = posPixelsY yInfo (nScore a.score)
        render a ctx = renderGlyph ctx $ do
          stroke (color a.chr)
          fill (color a.chr)
          circle { x: 0.0, y: 0.0 } 5.0


-- Renderers always run on collections (for now Arrays) of objects,
-- since it's rare to only want to render one single thing.
-- if that is desired, one can simply pass along a singleton array.
runRenderer :: forall a.
               Renderer _ a
            -> { xInfo :: XPosInfo, yInfo :: YPosInfo }
            -> Array a
            -> Context2D
            -> Eff _ Unit
runRenderer r {xInfo, yInfo} as ctx =
  for_ as (\a -> C.withContext ctx do
              let translateX = r.hPos a xInfo
                  translateY = r.vPos a yInfo
              _ <- C.translate {translateX, translateY} ctx
              r.render a ctx)

-- A Frame contains the information required to, in conjunction with a Renderer on `a`,
-- render the contained data to a given place on the canvas.

-- not going to be entirely trivial to construct this -- need to take the
-- total size, padding, etc. into account. not just a map; we need a fold, too.
-- ... a hylomorphism?

-- The actual size the contents have to be rendered to is equal to width - (2.0 * padding).
-- When rendered, individual frames are always immediately adjacent to one another
-- Likewise, the sum of `width`s in a Frames' is always equal to the total width the
-- Frames' can be drawn to.
type Frame a = { padding :: Pixels, width :: Pixels, contents :: a }

type ChrCtx r = Map ChrId (Record r)

type Frames' r a = ChrCtx (frame :: Frame a | r)

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


zipChrs :: forall a b c.
           Map ChrId a
        -> Map ChrId b
        -> Maybe ((a -> b -> c) -> Map ChrId c)
zipChrs a b =
  if chrIdsEq a b
     then let (Tuple chrs a') = Array.unzip $ Map.toAscUnfoldable a
              b' = snd <$> Map.toAscUnfoldable b
          in pure $ \f -> Map.fromFoldable $ Array.zip chrs $ Array.zipWith f a' b'
     else Nothing


renderFrames :: forall f a.
                YPosInfo
             -> Frames (Array a)
             -> Renderer _ a
             -> Context2D
             -> Eff _ Unit
renderFrames yInfo {offset, frames} r ctx = C.withContext ctx (foreachE frames f)
  where f {xInfo, contents} = do
            runRenderer r {xInfo, yInfo} contents ctx
            let len = bpToPixels xInfo.scale xInfo.size
            void $ C.translate {translateX: len+offset, translateY: 0.0} ctx


-- Commands the renderer thread can receive
data RenderCmds a =
    Render (Array { chrId :: ChrId, features :: Array a})
  | SetView View
  | Scroll Pixels


type RenderBackend = { canvas :: CanvasElement
                     , backCanvas :: CanvasElement
                     }
{-
renderer :: forall eff a.
            RenderBackend
         -> Renderer eff a
         -> AVar (RenderCmds a)
         -> StateT View (Aff _) Unit
renderer cvs r cmdVar = forever do
  cmd <- lift $ takeVar cmdVar
  case cmd of
    Render fs -> do
      -- let toRender :: Array (RenderParams -> Eff _ Unit)
      --     toRender = map r fs
      -- Use current (or provided?) View to derive RenderParams for each
      pure unit

    Scroll n -> do
      pure unit

    SetView v -> do
      State.put v
      pure unit
-}

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


testRender :: forall a.
              Renderer _ a
           -> Map ChrId (Array a)
           -> Aff _ Unit
testRender r chrs = do
  canvas <- liftEff $ unsafePartial $ fromJust <$> getCanvasElementById "canvas"
  ctx <- liftEff $ getContext2D canvas

  {w,h} <- liftEff do
    {w} <- getScreenSize
    h <- getCanvasHeight canvas
    _ <- setCanvasWidth (w-2.0) canvas
    pure {w, h}

  let height = h
      offset = 0.0
      yInfo = { height, offset }

      fs :: Frames (Array a)
      fs = frames w 20.0 chrs

  liftEff $ renderFrames yInfo fs r ctx


main = launchAff $ do
  let min = 0.1
      max = 0.4

      colors = fold $ Array.replicate 5 ["green", "yellow", "orange", "red", "purple", "blue"]
      chrColors :: _
      chrColors = Map.fromFoldable $ Array.zip mouseChrIds colors

      chrColor :: ChrId -> String
      chrColor chr = fromMaybe "black" (Map.lookup chr chrColors)

      render' :: Renderer _ _
      render' = gwasRenderer {min, max} chrColor

      chrSize :: _ -> Bp
      chrSize x = mouseChrSize (Bp 0.0) x.chr

  dat <- Array.fromFoldable <$> fetchGemma "./gwas.csv"

  let chrs :: _
      chrs = (\a -> let bp = chrSize $ NE.head a
                    in Tuple bp a) <$> Array.groupBy (\a b -> a.chr == b.chr) dat
      chrs' = map (fromNonEmpty (:)) <$> chrs

  liftEff $ log $ show $ (length chrs' :: Int)

  testRender render' chrs'
