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
import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Foldable (foldMap, for_, length)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe, Maybe(..), fromJust, fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala, alaF, unwrap, wrap)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import Debug.Trace (trace)
import Genetics.Browser.DataSource (DataSource)
import Genetics.Browser.Glyph (Glyph, circle, fill, stroke)
import Genetics.Browser.GlyphF.Canvas (renderGlyph)
import Genetics.Browser.Types (Bp(..), BpPerPixel(..), Chr, ChrId(..), Point, Range, bpToPixels)
import Genetics.Browser.UI.Native (getScreenSize)
import Genetics.Browser.UI.Native.View as View
import Genetics.Browser.View (View, Pixels)
import Global (readFloat)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getCanvasHeight, getContext2D, setCanvasWidth)
import Graphics.Canvas as C
import Math as Math
import Network.HTTP.Affjax as Affjax
import Partial.Unsafe (unsafePartial)


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


-- TODO the `offset` should probably be in XPosInfo,
-- and be the actual pixel offset for that bit on the canvas...
type Frames a = { offset :: Pixels
                , frames :: Array { xInfo :: XPosInfo
                                  , contents :: a
                                  } }


frames :: forall a.
          Pixels
       -> Pixels
       -> (a -> Bp)
       -> Array a
       -> Frames a
frames width offset f chrs = {offset, frames: g <$> chrs}
  where n = length chrs
        -- TODO the scale should depend on the current size; that needs more work
        totalSize = alaF Additive foldMap f chrs
        width' = width - offset * n
        g chr = let size = f chr
                    scale = BpPerPixel (unwrap totalSize / width')
                in {xInfo: {size, scale}, contents: chr}


mapFrames :: forall a b.
             (a -> b)
          -> Frames a
          -> Frames b
mapFrames f fs = fs { frames = (\a -> a { contents = f a.contents }) <$> fs.frames }


-- zipWithFrames :: forall a b.
--              Array (a -> b)
--           -> Frames a
--           -> Frames b
-- zipWithFrames funs fs = fs { frames = frames }
--   where frames = Array.zipWith (\fun frame -> fun <$> frame) funs fs.frames


renderFrames :: forall f a.
                YPosInfo
             -> Frames (Array a)
             -> Renderer _ a
             -> Context2D
             -> Eff _ Unit
renderFrames yInfo {offset, frames} r ctx = C.withContext ctx (foreachE frames f)
  where f {xInfo, contents} = do
            runRenderer r {xInfo, yInfo} contents ctx
            log $ "scale: " <> show (unwrap xInfo.scale)
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


testFetch :: forall r.
             String
          -> Aff _ (Array { score :: Number, pos :: Bp, chr :: Int })
testFetch url = do
  json <- _.response <$> Affjax.get url

  let f :: Json -> Maybe {score :: Number, pos :: Bp, chr :: Int }
      f j = do
            obj <- j ^? _Object
            pos <- Bp <$> obj ^? ix "min" <<< _Number
            pValue <- readFloat <$> obj ^? ix "pValue" <<< _String
            let score = (-1.0) * (Math.log pValue / Math.log 10.0)
                chr = 11
            pure {pos, score, chr}

  case traverse f =<< json ^? _Array of
    Nothing -> throwError $ error "Failed to parse JSON features"
    Just fs -> pure $ fs



testRender :: forall r a.
              Renderer _ { chr :: Int | r }
           -> Array { chr :: a | r }
           -> Aff _ Unit
testRender r as = do
  liftEff $ log "running"
  canvas <- liftEff $ unsafePartial $ fromJust <$> getCanvasElementById "canvas"
  ctx <- liftEff $ getContext2D canvas

  {w,h} <- liftEff do
    {w} <- getScreenSize
    h <- getCanvasHeight canvas
    _ <- setCanvasWidth (w-2.0) canvas
    pure {w, h}

  let size = Bp 122030190.0
      n = 10
      scale = BpPerPixel (unwrap size / (w / 10.0))
      xInfo = { size, scale }
      height = h
      offset = 0.0
      yInfo = { height, offset }

      as' :: _
      as' = Array.zipWith (\i as' -> _ {chr = i} <$> as') (1..5) (Array.replicate 5 as)

      fs :: Frames _
      fs = frames w 20.0 (const size) as'

  liftEff $ renderFrames yInfo fs r ctx


main = launchAff $ do
  dat <- testFetch "./gwas.json"
  let min = 0.0
      max = 50.0

      colors = ["green", "yellow", "orange", "red", "purple", "blue"]

      chrColor :: Int -> String
      chrColor i = fromMaybe "black" (colors !! i `mod` 5)

      render' :: _
      render' = gwasRenderer {min, max} chrColor

  testRender render' dat
