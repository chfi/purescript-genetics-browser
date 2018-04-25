module Genetics.Browser.Track.UI where

import Prelude

import Color (black)
import Control.Coroutine (Consumer, Producer, connect, runProcess)
import Control.Coroutine as Co
import Control.Monad.Aff (Aff, Fiber, Milliseconds, delay, forkAff, killFiber, launchAff, launchAff_)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, makeVar, putVar, readVar, takeVar, tryTakeVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Rec.Class (forever)
import DOM.Classy.ParentNode (toParentNode)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left))
import Data.Foldable (class Foldable, foldMap, length, null, sum)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Foreign (Foreign, MultipleErrors, renderForeignError)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', iso, re, to, united, (^.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Pair (Pair(..))
import Data.Pair as Pair
import Data.Profunctor.Star (Star(..))
import Data.Record.Extra (eqRecord)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_, traverse, traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Variant (Variant, case_, inj, on)
import Debug.Trace as Debug
import Genetics.Browser.Track.Backend (RenderedTrack, bumpFeatures, drawBrowser, negLog10, zipMapsWith)
import Genetics.Browser.Track.Demo (Annot, BedFeature, GWASFeature, Peak, annotForSnp, annotLegendTest, filterSig, getAnnotations, getAnnotations', getGWAS, getGenes, peaks, produceAnnots, produceGWAS, produceGenes, renderAnnot, renderAnnot', renderGWAS, visiblePeaks)
import Genetics.Browser.Track.UI.Canvas (BrowserCanvas, TrackPadding, _Dimensions, _Track, browserCanvas, browserOnClick, debugBrowserCanvas, dragScroll, renderBrowser, setBrowserCanvasSize, uiSlots, wheelZoom)
import Genetics.Browser.Types (Bp(Bp), ChrId(ChrId))
import Genetics.Browser.Types.Coordinates (CoordSys, CoordSysView(..), ViewScale(..), _TotalSize, aroundPair, coordSys, normalizeView, pairSize, pairsOverlap, pixelsView, scaleViewBy, showViewScale, translateViewBy, viewScale)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Drawing, Point)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Simple.JSON (read)



foreign import windowInnerSize :: ∀ e. Eff e Canvas.Dimensions

-- | Set an event to fire on the given button id
foreign import buttonEvent :: ∀ e.
                              String
                           -> Eff e Unit
                           -> Eff e Unit

-- | Set callback to run after the window has resized (see UI.js for
-- | time waited), providing it with the new window size.
foreign import resizeEvent :: ∀ e.
                              ({ width  :: Number
                               , height :: Number } -> Eff e Unit)
                           -> Eff e Unit

data UpdateView =
    ScrollView Number
  | ZoomView Number
  | ModView (Pair BigInt -> Pair BigInt)


instance showUpdateView :: Show UpdateView where
  show (ScrollView x) = "(Scroll by " <> show x <> ")"
  show (ZoomView s) = "(Zoom by " <> show s <> ")"
  show _ = "(ModView)"


    -- TODO idk if this instance makes sense??? whatevs
instance semigroupUpdateView :: Semigroup UpdateView where
  append (ScrollView x1) (ScrollView x2) = ScrollView (x1 + x2)
  append (ZoomView s1)   (ZoomView s2)   = ZoomView   (s1 * s2)
  append _ y = y

instance monoidUpdateView :: Monoid UpdateView where
  mempty = ModView id


updateViewFold :: UpdateView
               -> CoordSysView
               -> CoordSysView
updateViewFold uv iv = case uv of
  ZoomView   x -> iv `scaleViewBy`     x
  ScrollView x -> iv `translateViewBy` x
  ModView f    -> over CoordSysView f iv


queueCmd :: ∀ a. AVar a -> a -> Eff _ Unit
queueCmd av cmd = launchAff_ $ putVar cmd av


btnScroll :: Number -> AVar UpdateView -> Eff _ Unit
btnScroll x av = do
  buttonEvent "scrollLeft"  $ queueCmd av $ ScrollView (-x)
  buttonEvent "scrollRight" $ queueCmd av $ ScrollView   x


btnZoom :: Number -> AVar UpdateView -> Eff _ Unit
btnZoom x av = do
  buttonEvent "zoomOut" $ queueCmd av $ ZoomView $ 1.0 + x
  buttonEvent "zoomIn"  $ queueCmd av $ ZoomView $ 1.0 - x



mouseChrSizes :: Array (Tuple ChrId BigInt)
mouseChrSizes =
  unsafePartial
  $ map (bimap ChrId (fromJust <<< BigInt.fromString))
      [ Tuple "1"   "195471971"
      , Tuple "2"   "182113224"
      , Tuple "3"   "160039680"
      , Tuple "4"   "156508116"
      , Tuple "5"   "151834684"
      , Tuple "6"   "149736546"
      , Tuple "7"   "145441459"
      , Tuple "8"   "129401213"
      , Tuple "9"   "124595110"
      , Tuple "10"  "130694993"
      , Tuple "11"  "122082543"
      , Tuple "12"  "120129022"
      , Tuple "13"  "120421639"
      , Tuple "14"  "124902244"
      , Tuple "15"  "104043685"
      , Tuple "16"  "98207768"
      , Tuple "17"  "94987271"
      , Tuple "18"  "90702639"
      , Tuple "19"  "61431566"
      , Tuple "X"   "171031299"
      , Tuple "Y"   "91744698"
      ]

-- runs console.time() with the provided string, returns the effect to stop the timer
foreign import timeEff :: ∀ eff. String -> Eff eff (Eff eff Unit)



type UICmdR = ( render :: Unit
              , docResize :: { width :: Number, height :: Number } )

_render = SProxy :: SProxy "render"
_docResize = SProxy :: SProxy "docResize"



type UIState e = { view         :: AVar CoordSysView
                 , viewCmd      :: AVar UpdateView
                 , uiCmd        :: AVar (Variant UICmdR)
                 , renderFiber  :: AVar (Fiber e Unit)
                 , lastOverlaps :: AVar (Number -> Point -> { gwas :: Array (GWASFeature ()) } )
                 }


diffView :: CoordSysView -> CoordSysView -> CoordSysView
diffView v1 v2 =
  let (Pair l1 r1) = unwrap v1
      (Pair l2 r2) = unwrap v2
  in wrap $ Pair (l2 - l1) (r2 - r1)

showView :: CoordSysView -> String
showView (CoordSysView (Pair l r)) = BigInt.toString l <> " - " <> BigInt.toString r


_PairRec :: ∀ a. Iso' (Pair a) { l :: a, r :: a }
_PairRec = iso (\(Pair l r) -> {l, r}) (\ {l, r} -> Pair l r)


debugView :: UIState _
          -> Eff _ { get :: _
                   , set :: _ }
debugView s = do
  let get name = launchAff_ do
         view <- AVar.tryReadVar s.view
         liftEff case view of
           Nothing -> do
             log "CoordSysView state empty"
             setWindow name unit
           Just (v :: _)  -> do
             log $ "CoordSysView: " <> showView v
             setWindow name $ unwrap v ^. _PairRec

  let set lr = launchAff_ do
         view <- AVar.tryTakeVar s.view
         case view of
           Nothing -> pure unit
           Just _  -> do
             let v' = wrap $ lr ^. re _PairRec
             _ <- AVar.tryPutVar (inj _render unit) s.uiCmd
             AVar.putVar v' s.view

  pure {get, set}


uiViewUpdate :: ∀ r.
                CoordSys _ BigInt
             -> Milliseconds
             -> { view :: AVar CoordSysView
                , viewCmd :: AVar UpdateView
                , uiCmd :: AVar (Variant UICmdR) | r }
                -- , viewReady :: AVar Unit | r }
             -> Aff _ Unit
uiViewUpdate cs timeout { view, viewCmd, uiCmd } = do
  curCmdVar <- makeVar (ModView id)

  let normView = normalizeView cs (BigInt.fromInt 200000)
      loop' updater = do
        cmd <- takeVar viewCmd

        killFiber (error "Resetting view update") updater
        liftEff $ log $ "forking view update"

        curCmd <- takeVar curCmdVar
        let cmd' = curCmd <> cmd
        putVar cmd' curCmdVar

        updater' <- forkAff do
            delay timeout
            liftEff $ log "Running view update"
            vr <- takeVar view

            let vr' = normView $ updateViewFold cmd' vr

            putVar vr' view

            putVar (inj _render unit) uiCmd
            takeVar curCmdVar *> putVar mempty curCmdVar

        loop' updater'

  loop' (pure unit)





                         -- This is Star (Aff e) a b!
-- newtype AffCache e a b = AffCache (a -> Aff e b)


affCacheStar :: ∀ e a b.
                (a -> a -> Boolean)
             -> (a -> b)
             -> Aff _ (Star (Aff _) a b)
affCacheStar diff f = do
  inVar  <- AVar.makeEmptyVar
  outVar <- AVar.makeEmptyVar

  let cache = \a -> do
        lastIn  <- AVar.tryTakeVar inVar
        lastOut <- AVar.tryTakeVar outVar
        let o = case lastIn, lastOut of
                  Just i, Just o -> if diff a i then f a else o
                  _, _ -> f a

        AVar.putVar a inVar
        AVar.putVar o outVar
        pure o

  pure $ Star cache

cachedStar :: ∀ a b. Star (Aff _) a b -> a -> Aff _ b
cachedStar = unwrap



newtype AffCache e a b =
  AffCache { diff :: a -> a -> Boolean
           , cache :: a -> Aff e b }

derive instance newtypeAffCache :: Newtype (AffCache e a b) _
derive instance genericAffCache :: Generic (AffCache e a b) _

derive instance functorAffCache :: Functor (AffCache e a)

cacheFun :: ∀ e a b.
            (a -> a -> Boolean)
         -> (a -> b)
         -> Aff _ (AffCache _ a b)
cacheFun diff f = do
  inVar  <- AVar.makeEmptyVar
  outVar <- AVar.makeEmptyVar

  let cache = \a -> do
        lastIn  <- AVar.tryTakeVar inVar
        lastOut <- AVar.tryTakeVar outVar
        let o = case lastIn, lastOut of
                  Just i, Just o -> if diff a i then f a else o
                  _, _ -> f a

        AVar.putVar a inVar
        AVar.putVar o outVar
        pure o

  pure $ AffCache { diff, cache }


cached :: ∀ a b.
          AffCache _ a b
       -> a -> Aff _ b
cached = _.cache <<< unwrap




browserCache
  :: ∀ a b.
     (BrowserCanvas
      -> CoordSysView
      -> { tracks :: { gwas :: a
                     , annotations :: b }
         , relativeUI :: Drawing
         , fixedUI :: Drawing })
     -> Aff _ (BrowserCanvas
               -> CoordSysView
               -> Aff _ ({ tracks :: { gwas :: a
                                     , annotations :: b }
                         , relativeUI :: Drawing
                         , fixedUI :: Drawing }))
browserCache f = do
  bcDim <- AVar.makeEmptyVar
  viewSize <- AVar.makeEmptyVar

  lastPartial <- AVar.makeEmptyVar
  lastFinal <- AVar.makeEmptyVar

  pure \bc csv -> do

    let newBCDim = bc ^. _Dimensions
    oldBCDim <- AVar.tryTakeVar bcDim
    AVar.putVar newBCDim bcDim

    oldPartial <- AVar.tryTakeVar lastPartial

    partial <- case oldBCDim, oldPartial of
      Just d, Just o -> do
        let changed = not $ d `eqRecord` newBCDim
        liftEff $ log $ "BrowserCanvas changed? " <> (show changed)
        -- remove the cached final output value on canvas resize,
        -- since we need to recalculate everything
        when changed (void $ AVar.tryTakeVar lastFinal)
        pure $ if d `eqRecord` newBCDim then o else f bc

      _, _ -> do
        liftEff $ log "Cache was empty! Recalculating all"
        _ <- AVar.tryTakeVar lastFinal
        pure $ f bc

    AVar.putVar partial lastPartial

    let newViewSize = pairSize $ unwrap csv
    oldViewSize <- AVar.tryTakeVar viewSize
    AVar.putVar newViewSize viewSize

    oldFinal <- AVar.tryTakeVar lastFinal

    output <- case oldViewSize, oldFinal of
      Just vs, Just o -> do
        let changed = newViewSize /= vs
        liftEff $ log $ "CoordSysView changed? " <> (show changed)
        pure $ if not changed then o else partial csv

      _, _ -> do
        liftEff $ log "Cache was empty! Recalculating tracks"
        pure $ partial csv




    AVar.putVar output lastFinal

    pure output



renderLoop :: CoordSys _ _
           -> (BrowserCanvas
               -> CoordSysView
               -> Aff _ ({ tracks ::
                              { gwas :: RenderedTrack (GWASFeature ())
                              , annotations :: RenderedTrack (Annot (score :: Number)) }
                         , relativeUI :: Drawing
                         , fixedUI :: Drawing }))
           -> BrowserCanvas
           -> UIState _
           -> Aff _ Unit
renderLoop cSys drawCachedBrowser canvas state = forever do

  uiCmd <- takeVar state.uiCmd
  case_ # on _render (\_ -> pure unit)
        # on _docResize (\ {width} -> do
                let {height} = canvas ^. _Dimensions
                canvas' <- liftEff $ setBrowserCanvasSize {width, height} canvas
                putVar (inj _render unit) state.uiCmd
                renderLoop cSys drawCachedBrowser canvas' state)
        $ uiCmd

  -- if there's a rendering fiber running, we kill it
  traverse_ (killFiber (error "Resetting renderer"))
    =<< tryTakeVar state.renderFiber


  csView <- readVar state.view
  toRender <- drawCachedBrowser canvas csView

  let currentScale = viewScale (canvas ^. _Track <<< _Dimensions) csView
      (Pair offset _) = pixelsView currentScale csView

  _ <- AVar.tryTakeVar state.lastOverlaps
                    -- offset the click by the current canvas translation
  AVar.putVar (\n r -> let r' = {x: r.x + offset, y: r.y }
                       in { gwas: toRender.tracks.gwas.overlaps n r' }) state.lastOverlaps

  -- fork a new renderFiber
  renderFiber <- forkAff
                 $ renderBrowser (wrap 2.0) canvas offset toRender

  putVar renderFiber state.renderFiber



printSNPInfo :: ∀ r. Array (GWASFeature r) -> Eff _ Unit
printSNPInfo fs = do
  let n = length fs :: Int
      m = 5

      showSnp f = do
        log $ f.feature.name <> " - "
           <> unwrap f.feature.chrId <> " @ "
           <> show (map show f.position)

  log $ "showing " <> show n <> " clicked glyphs"
  for_ (Array.take m fs) showSnp


snpInfoHTML :: ∀ r. GWASFeature r -> String
snpInfoHTML { position, feature } =
    "<p>SNP: "   <> feature.name <> "</p>"
 <> "<p>Chr: "   <> show feature.chrId <> "</p>"
 <> "<p>Pos: "   <> show (Pair.fst position) <> "</p>"
 <> "<p>Score: " <> show feature.score <> "</p>"
 <> "<p>-log10: " <> show (negLog10 feature.score) <> "</p>"


snpInfoHTML' :: ∀ rA rS.
                (GWASFeature rS -> Maybe (Annot (score :: Number | rA)))
             -> GWASFeature rS
             -> String
snpInfoHTML' assocAnnot snp =
   snpInfoHTML snp <> case assocAnnot snp of
     Nothing -> "<p>No annotation found</p>"
     Just a  -> "<p>Annotation: " <> show a.feature.name <> "</p>"
             <> "<p>Annot. score: " <> show a.feature.score <> "</p>"
             <> "<p>Annot. -log10: " <> show (negLog10 a.feature.score) <> "</p>"


foreign import setInfoBoxVisibility :: ∀ e. String -> Eff e Unit
foreign import setInfoBoxContents :: ∀ e. String -> Eff e Unit

foreign import initDebugDiv :: ∀ e. Number -> Eff e Unit


foreign import setDebugDivVisibility :: ∀ e. String -> Eff e Unit

foreign import setDebugDivPoint :: ∀ e. Point -> Eff e Unit



observeViewScale :: ∀ r1 r2 .
                    AVar CoordSysView
                 -> BrowserCanvas
                 -> Aff _ ViewScale
observeViewScale csv bc =
  viewScale (bc ^. _Dimensions) <$> AVar.readVar csv



-- TODO configure UI widths
runBrowser :: Conf -> BrowserCanvas -> Eff _ _
runBrowser config bc = launchAff $ do

  let clickRadius = 1.0

  liftEff $ initDebugDiv clickRadius

  let cSys :: CoordSys ChrId BigInt
      cSys = coordSys mouseChrSizes

      bumpRadius = Bp 50000000.0

  trackData <- do
    genes <- traverse (getGenes cSys) config.urls.genes
    gwas  <- traverse (getGWAS  cSys) config.urls.gwas

    rawAnnotations <-
      traverse (uncurry $ getAnnotations' cSys) do
        snps <- filterSig config.score <$> gwas
        url <- config.urls.annotations
        pure $ Tuple snps url

    let annotations = zipMapsWith
                       (bumpFeatures (to _.feature.score)
                                      (SProxy :: SProxy "score")
                                      bumpRadius)
                       <$> gwas <*> rawAnnotations

    pure { genes, gwas, annotations }

  let initialView :: CoordSysView
      initialView = wrap $ Pair zero (cSys^._TotalSize)
      trackDims = bc ^. _Track <<< _Dimensions

      peaks' :: ViewScale -> Map ChrId (Array (Peak _ _ _))
      peaks' vs = visiblePeaks vs
                  $ filterSig config.score
                  $ fromMaybe mempty trackData.gwas

  viewCmd <- makeEmptyVar

  liftEff do
    btnScroll 0.05 viewCmd
    btnZoom   0.10 viewCmd

    let dragCB {x,y} =
          when (Math.abs x >= one)
          $ queueCmd viewCmd $ ScrollView $ (-x) / trackDims.width
    dragScroll bc dragCB


    let scrollZoomScale = 0.06
        wheelCB dY =
          queueCmd viewCmd $ ZoomView $ 1.0 + scrollZoomScale * dY
    wheelZoom bc wheelCB


  uiCmd <- makeVar (inj _render unit)

  liftEff do
    resizeEvent \d ->
      queueCmd uiCmd (inj _docResize d)


  view <- makeVar initialView
  renderFiber <- makeEmptyVar
  lastOverlaps <- AVar.makeEmptyVar

  let initState :: UIState _
      initState = { view, viewCmd, uiCmd, renderFiber, lastOverlaps }

  let
      slots = uiSlots bc

      entries = foldMap annotLegendTest trackData.annotations
      legend = { width: slots.right.size.width
               , entries }

      s = config.score


      vscale = { width: slots.left.size.width
               , color: black
               , min: s.min, max: s.max, sig: s.sig }


      sigSnps = fromMaybe mempty $ filterSig config.score <$> trackData.gwas

      mainBrowser :: _
      mainBrowser = drawBrowser cSys {legend, vscale}
                      { gwas: renderGWAS vscale
                      , annotations: renderAnnot' cSys sigSnps vscale }
                      { gwas: fromMaybe mempty trackData.gwas
                      , annotations: fromMaybe mempty trackData.annotations }



      viewTimeout :: Milliseconds
      viewTimeout = wrap 100.0

  liftEff do
    setWindow "mainBrowser" mainBrowser

    let findAnnot = annotForSnp bumpRadius $ fromMaybe mempty trackData.annotations

    let glyphClick :: _
        glyphClick p = launchAff_ do
          AVar.tryReadVar lastOverlaps >>= case _ of
             Nothing -> liftEff do
               log "clicked no glyphs"
               setInfoBoxVisibility "hidden"

             Just gs -> liftEff do
               let clicked = (gs clickRadius p).gwas
               printSNPInfo clicked
               case Array.head clicked of
                 Nothing -> setInfoBoxVisibility "hidden"
                 Just g  -> do
                   setInfoBoxContents $ snpInfoHTML' findAnnot g
                   setInfoBoxVisibility "visible"


    let overlayDebug :: _
        overlayDebug p = do

          setDebugDivVisibility "visible"
          setDebugDivPoint p


    browserOnClick bc
      { overlay: \_ -> pure unit
      -- { overlay: overlayDebug
      , track:   glyphClick }


  liftEff $ setWindow "peaks" $ launchAff_ do
    vs <- observeViewScale view bc
    let ps = peaks' vs
    liftEff do
      forWithIndex_ ps \chrId p -> do
        log $ show chrId <> " peaks: " <> show (Array.length p)

  _ <- forkAff $ uiViewUpdate cSys viewTimeout initState

  cached <- browserCache mainBrowser

  _ <- forkAff $ renderLoop cSys cached bc initState

  pure unit



type DataURLs = { gwas        :: Maybe String
                , annotations :: Maybe String
                , genes       :: Maybe String
                }


type Conf = { browserHeight :: Number
            , trackPadding :: TrackPadding
            , score :: { min :: Number, max :: Number, sig :: Number }
            , urls :: DataURLs
            }

foreign import setWindow :: ∀ e a. String -> a -> Eff e Unit


initBrowser :: Foreign -> Eff _ _
initBrowser rawConfig = do

  el' <- do
    doc <- DOM.htmlDocumentToDocument
           <$> (DOM.document =<< DOM.window)
    DOM.querySelector (wrap "#browser") (toParentNode doc)

  case read rawConfig :: Either MultipleErrors Conf of
    Left errs -> traverse_ (log <<< renderForeignError) errs
    Right c   -> do
      case el' of
        Nothing -> log "Could not find element '#browser'"
        Just el -> do

          {width} <- windowInnerSize
          let dimensions = { width, height: c.browserHeight }
          bc <- browserCanvas dimensions c.trackPadding el

          debugBrowserCanvas "debugBC" bc

          log $ unsafeStringify c
          void $ runBrowser c bc



-- TODO this could almost certainly be done better
chunkConsumer :: ∀ m a.
                 Foldable m
              => Monoid (m a)
              => AVar (m a)
              -> Consumer (m a) (Aff _) Unit
chunkConsumer av = Co.consumer \m ->
  if null m
     then pure $ Just unit
     else do
       sofar <- takeVar av
       putVar (sofar <> m) av
       delay (wrap 20.0)
       pure Nothing


type TrackVar a = AVar (Map ChrId (Array a))
type TrackProducer eff a = Producer (Map ChrId (Array a)) (Aff eff) Unit

-- Feels like this one takes care of a bit too much...
fetchLoop1 :: ∀ a.
              (Maybe (Aff _ (TrackProducer _ a)))
           -> Aff _ (TrackVar a)
fetchLoop1 Nothing = AVar.makeEmptyVar
fetchLoop1 (Just startProd) = do
  prod <- startProd
  avar <- makeVar mempty
  _ <- forkAff $ runProcess $ prod `connect` chunkConsumer avar
  pure avar

-- | Starts threads that fetch & parse each of the provided tracks,
-- | filling an AVar over time per track, which can be used by other parts of the application
-- | (read only, should be a newtype)
fetchLoop :: CoordSys ChrId BigInt
          -> DataURLs
          -> Aff _
               { gwas :: TrackVar (GWASFeature ())
               , genes :: TrackVar BedFeature
               , annotations :: TrackVar (Annot ())
               }
fetchLoop cs urls = do
  gwas <-        fetchLoop1 $ produceGWAS   cs <$> urls.gwas
  annotations <- fetchLoop1 $ produceAnnots cs <$> urls.annotations
  genes <-       fetchLoop1 $ produceGenes  cs <$> urls.genes
  pure { gwas, genes, annotations }
