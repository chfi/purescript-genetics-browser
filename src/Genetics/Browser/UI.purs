module Genetics.Browser.UI where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left))
import Data.Filterable (filterMap)
import Data.Foldable (foldMap, length)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Lens ((^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (over, unwrap, wrap)
import Data.Pair (Pair(..))
import Data.Pair as Pair
import Data.Symbol (SProxy(SProxy))
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Variant (Variant, case_, inj)
import Data.Variant as V
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds, delay, forkAff, killFiber, launchAff, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throw)
import Foreign (Foreign, MultipleErrors, renderForeignError)
import Genetics.Browser (LegendConfig, Peak, VScale, pixelSegments)
import Genetics.Browser.Canvas (BrowserContainer, TrackPadding, _Container, browserClickHandler, browserContainer, dragScroll, getDimensions, getLayers, setBrowserContainerSize, setElementStyle, wheelZoom)
import Genetics.Browser.Coordinates (CoordSys, CoordSysView(CoordSysView), _TotalSize, _Segments, coordSys, normalizeView, pairsOverlap, scalePairBy, scaleToScreen, translatePairBy, viewScale)
import Genetics.Browser.Demo (Annotation, AnnotationField, SNP, SNPConfig, AnnotationsConfig, addDemoLayers, annotationsForScale, filterSig, getAnnotations, getGenes, getSNPs, showAnnotationField)
import Genetics.Browser.Layer (Component(Padded), browserSlots)
import Genetics.Browser.Types (ChrId(ChrId), _NegLog10, _prec)
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Point)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Simple.JSON (read)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Document (createElement, documentElement) as DOM
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (setId)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.DOM.ParentNode (querySelector) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument) as DOM
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent (key) as DOM


foreign import windowInnerSize :: Effect Canvas.Dimensions

-- | Set an event to fire on the given button id
foreign import buttonEvent :: String
                           -> Effect Unit
                           -> Effect Unit

foreign import keydownEvent :: Element
                            -> (KeyboardEvent -> Effect Unit)
                            -> Effect Unit

-- | Set callback to run after the window has resized (see UI.js for
-- | time waited), providing it with the new window size.
foreign import resizeEvent :: ({ width  :: Number
                               , height :: Number } -> Effect Unit)
                           -> Effect Unit


-- | The value returned by the function that initializes the genome browser;
-- | provides an interface to the running browser instance.
type BrowserInterface a
  = { getView          :: Aff CoordSysView
    , getBrowserCanvas :: Aff BrowserContainer
    , lastHotspots     :: Effect (Number -> Point -> Array a)
    , queueCommand     :: Variant UICmdR -> Aff Unit
    , queueUpdateView  :: UpdateView -> Effect Unit
    }


-- | Creates the browser using the provided initial data, returning
-- | a BrowserInterface for reading state & sending commands to it
initializeBrowser :: ∀ c r.
                     CoordSys c BigInt
                  -> { snps        :: Number -> CoordSysView -> Effect Unit
                     , annotations :: Number -> CoordSysView -> Effect Unit
                     , chrs        :: Number -> CoordSysView -> Effect Unit
                     , hotspots    :: Effect (Number -> Point -> Array (SNP ()))
                     , fixedUI     :: Effect Unit | r}
                  -> CoordSysView
                  -> BrowserContainer
                  -> Aff (BrowserInterface (SNP ()))
initializeBrowser cSys renderFuns initView bc = do

  renderFiberVar <- AVar.empty

  viewVar <- AVar.new initView
  bcVar <- AVar.new bc

  uiCmdVar <- AVar.empty
  lastHotspotsVar <- AVar.empty

  let getView = AVar.read viewVar
      getBrowserCanvas = AVar.read bcVar
      queueCommand = flip AVar.put uiCmdVar
      lastHotspots = renderFuns.hotspots

    -- hardcoded timeout for now
  queueUpdateView <- uiViewUpdate cSys (wrap 100.0) {view: viewVar, uiCmd: uiCmdVar}

  let mainLoop = do
        uiCmd <- AVar.take uiCmdVar
        case_ # V.on _render (\_ -> pure unit)
              # V.on _docResize (\ {width} -> do
                      {height} <- _.size <$> getDimensions bc
                      setBrowserContainerSize {width, height} bc
                      queueCommand $ inj _render unit
                      mainLoop)
              $ uiCmd

        -- if there's a rendering fiber running, we kill it
        traverse_ (killFiber (error "Resetting renderer"))
          =<< AVar.tryTake renderFiberVar

        csView <- getView
        canvas <- getBrowserCanvas

        currentDims <- getDimensions canvas

        let trackDims = _.padded $ browserSlots currentDims
            currentScale = viewScale trackDims.size csView
            offset = scaleToScreen  currentScale (Pair.fst $ unwrap $ csView)

        -- fork a new renderFiber
        renderFiber <- forkAff $ liftEffect do
          log $ "rendering with offset: " <> show offset
          log <<< show =<< Map.keys <$> getLayers bc

          renderFuns.chrs        offset csView
          renderFuns.annotations offset csView
          renderFuns.snps        offset csView
          renderFuns.fixedUI

        AVar.put renderFiber renderFiberVar

        mainLoop

  _ <- forkAff mainLoop

  queueCommand $ inj _render unit

  pure { getView
       , getBrowserCanvas
       , lastHotspots
       , queueCommand
       , queueUpdateView }


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
  mempty = ModView identity


updateViewFold :: UpdateView
               -> CoordSysView
               -> CoordSysView
updateViewFold uv = over CoordSysView case uv of
  ZoomView   x -> (_ `scalePairBy`     x)
  ScrollView x -> (_ `translatePairBy` x)
  ModView f    -> f


queueCmd :: ∀ a. AVar a -> a -> Effect Unit
queueCmd av cmd = launchAff_ $ AVar.put cmd av


-- | Provided with the AVar containing the view state (which is updated atomically)
-- | and the UICmd AVar queue (which is only written to),
-- | return a function that queues view updates using the provided timeout.
uiViewUpdate :: ∀ c r.
                CoordSys c BigInt
             -> Milliseconds
             -> { view  :: AVar CoordSysView
                , uiCmd :: AVar (Variant UICmdR) | r }
             -> Aff (UpdateView -> Effect Unit)
uiViewUpdate cs timeout { view, uiCmd } = do
  viewCmd <- AVar.empty

  curCmdVar <- AVar.new (ModView identity)

  let normView = normalizeView cs (BigInt.fromInt 200000)
      loop' updater = do
        cmd <- AVar.take viewCmd

        killFiber (error "Resetting view update") updater
        liftEffect $ log $ "forking view update"

        curCmd <- AVar.take curCmdVar
        let cmd' = curCmd <> cmd
        AVar.put cmd' curCmdVar

        updater' <- forkAff do
            delay timeout
            liftEffect $ log "Running view update"

            vr <- AVar.take view
            let vr' = normView $ updateViewFold cmd' vr
            AVar.put vr' view

            AVar.put (inj _render unit) uiCmd
            AVar.take curCmdVar *> AVar.put mempty curCmdVar

        loop' updater'

  _ <- forkAff $ loop' (pure unit)

  pure $ queueCmd viewCmd



btnUI :: { scrollMod :: Number, zoomMod :: Number }
      -> (UpdateView -> Effect Unit)
      -> Effect Unit
btnUI mods cb = do
  buttonEvent "scrollLeft"  $ cb $ ScrollView     (-mods.scrollMod)
  buttonEvent "scrollRight" $ cb $ ScrollView       mods.scrollMod
  buttonEvent "zoomOut"     $ cb $ ZoomView $ 1.0 + mods.zoomMod
  buttonEvent "zoomIn"      $ cb $ ZoomView $ 1.0 - mods.zoomMod


keyUI :: ∀ r.
         Element
      -> { scrollMod :: Number | r }
      -> (UpdateView -> Effect Unit)
      -> Effect Unit
keyUI el mods cb = keydownEvent el f
  where f ke = case DOM.key ke of
          "ArrowLeft"  -> cb $ ScrollView (-mods.scrollMod)
          "ArrowRight" -> cb $ ScrollView   mods.scrollMod
          _ -> pure unit


type UICmdR = ( render :: Unit
              , docResize :: { width :: Number, height :: Number } )

_render = SProxy :: SProxy "render"
_docResize = SProxy :: SProxy "docResize"


debugView :: ∀ a.
             BrowserInterface a
          -> Effect { get :: String -> Effect Unit
                    , set :: { l :: Number, r :: Number } -> Effect Unit }
debugView s = unsafePartial do
  let get name = launchAff_ do
         view <- s.getView
         liftEffect do
           log $ "CoordSysView: " <> show (map BigInt.toString $ unwrap view)
           setWindow name $ (\(Pair l r) -> {l,r}) $ unwrap view

  let set lr = s.queueUpdateView
               $ ModView $ const ((fromJust <<< BigInt.fromNumber) <$> Pair lr.l lr.r)

  pure {get, set}



printSNPInfo :: ∀ r. Array (SNP r) -> Effect Unit
printSNPInfo fs = do
  let n = length fs :: Int
      m = 5
  log $ "showing " <> show m <> " out of " <> show n <> " clicked glyphs"
  for_ (Array.take m fs) (log <<< unsafeCoerce)


wrapWith :: String -> String -> String
wrapWith tag x =
  "<"<> tag <>">" <> x <> "</"<> tag <>">"

snpHTML :: ∀ r.
           SNP r
        -> String
snpHTML {position, feature} = wrapWith "div" contents
  where contents = foldMap (wrapWith "p")
            [ "SNP: "    <> feature.name
            , "Chr: "    <> show feature.chrId
            , "Pos: "    <> show (Pair.fst position)
            , "-log10: " <> feature.score ^. _NegLog10 <<< _Newtype <<< _prec 4
            ]


peakHTML :: ∀ a b c.
            (a -> String)
         -> Peak b c a
         -> String
peakHTML disp peak =
  case Array.uncons peak.elements of
    Nothing               -> ""
    Just {head, tail: []} -> disp head
    Just {head, tail}     ->
      wrapWith "div" $ wrapWith "p"
        $ show (length tail + 1) <> " annotations"


annoPeakHTML :: ∀ a b.
                Peak a b (Annotation ())
             -> String
annoPeakHTML peak =
  case Array.uncons peak.elements of
    Nothing               -> ""
    Just {head, tail: []} -> annotationHTMLAll head
    Just {head, tail}     -> wrapWith "div"
                             ( wrapWith "p" "Annotations:"
                             <> foldMap annotationHTMLShort peak.elements)


-- | Given a function to transform the data in the annotation's "rest" field
-- | to text (or Nothing if the field should not be displayed), produce a
-- | function that generates HTML from annotations
annotationHTML :: (AnnotationField -> Maybe String)
                -> Annotation () -> String
annotationHTML disp {feature} = wrapWith "div" contents
  where url = fromMaybe "No URL"
              $ map (\a -> "URL: <a target='_blank' href='"
                           <> a <> "'>" <> a <> "</a>") feature.url

        name = fromMaybe ("Annotated SNP: " <> feature.name)
                         (("Gene: " <> _) <$> feature.gene)

        showOther fv = fv.field <> ": " <> (unsafeCoerce fv.value)

        contents = foldMap (wrapWith "p")
          $ [ name
            , url
            ] <> (filterMap disp
                  $ Array.fromFoldable feature.rest)

-- | Shows all data in "rest" using the default showAnnotationField (which uses unsafeCoerce)
annotationHTMLAll :: Annotation () -> String
annotationHTMLAll =
  annotationHTML (pure <<< showAnnotationField)


annotationHTMLDefault :: Annotation () -> String
annotationHTMLDefault = annotationHTML \x -> pure case x of
  {field: "p_lrt", value} ->
     "p_lrt: " <> (unsafeCoerce value) ^. _NegLog10 <<< _Newtype <<< _prec 4
  fv -> showAnnotationField fv

annotationHTMLShort :: Annotation () -> String
annotationHTMLShort {feature} = wrapWith "p" anchor
  where
        name' = fromMaybe (feature.name)
                          (feature.gene)

        showOther fv = fv.field <> ": " <> (unsafeCoerce fv.value)

        anchor = case feature.url of
          Nothing  -> name'
          Just url -> "<a target='_blank' href='" <> url <> "'>" <> name' <> "</a>"



foreign import initDebugDiv :: Number -> Effect Unit
foreign import setDebugDivVisibility :: String -> Effect Unit
foreign import setDebugDivPoint :: Point -> Effect Unit


foreign import setElementContents :: Element -> String -> Effect Unit

data InfoBoxF
  = IBoxShow
  | IBoxHide
  | IBoxSetY Int
  | IBoxSetX Int
  | IBoxSetContents String

derive instance genericInfoBoxF :: Generic InfoBoxF _

instance showInfoBoxF :: Show InfoBoxF where
  show = genericShow

updateInfoBox :: Element -> InfoBoxF -> Effect Unit
updateInfoBox el cmd =
  case cmd of
    IBoxShow ->
      setElementStyle el "visibility" "visible"
    IBoxHide ->
      setElementStyle el "visibility" "hidden"
    (IBoxSetX x)    ->
      setElementStyle el "left" $ show x <> "px"
    (IBoxSetY y)    ->
      setElementStyle el "top"  $ show y <> "px"
    (IBoxSetContents html) ->
      setElementContents el html

infoBoxId :: String
infoBoxId = "infoBox"

initInfoBox :: Effect (InfoBoxF -> Effect Unit)
initInfoBox = do
  doc <- map DOM.toDocument
           $ DOM.document =<< DOM.window
  el <- DOM.createElement "div" doc

  setId infoBoxId el

  DOM.documentElement doc >>= case _ of
    Nothing -> throw "Couldn't find document body!"
    Just docBody -> void $ appendChild (Element.toNode el) (Element.toNode docBody)

  pure $ updateInfoBox el


runBrowser :: Conf -> BrowserContainer -> Effect (Fiber Unit)
runBrowser config bc = launchAff $ do

  let cSys :: CoordSys ChrId BigInt
      cSys = coordSys mouseChrSizes

      initialView = fromMaybe (wrap $ Pair zero (cSys^._TotalSize)) do
        {left, right} <- config.initialChrs
        (Pair l _) <- Map.lookup (wrap left) $ cSys^._Segments
        (Pair _ r) <- Map.lookup (wrap right) $ cSys^._Segments
        pure $ wrap $ Pair l r

      clickRadius = 1.0

  liftEffect $ initDebugDiv clickRadius

  cmdInfoBox <- liftEffect $ initInfoBox


  trackData <-
    {genes:_, snps:_, annotations:_}
    <$> foldMap (getGenes       cSys) config.urls.genes
    <*> foldMap (getSNPs        cSys) config.urls.snps
    <*> foldMap (getAnnotations cSys) config.urls.annotations


  render <- liftEffect
            $ addDemoLayers cSys config trackData bc

  browser <-
    initializeBrowser cSys render initialView bc


  liftEffect do
    resizeEvent \d ->
      launchAff_ $ browser.queueCommand $ inj _docResize d

    let btnMods = { scrollMod: 0.05, zoomMod: 0.1 }
    btnUI btnMods browser.queueUpdateView

    buttonEvent "reset"
      $ browser.queueUpdateView
        (ModView (const $ unwrap initialView))

    keyUI (bc ^. _Container) { scrollMod: 0.075 } browser.queueUpdateView

    dragScroll bc \ {x,y} ->
      -- only do anything when scrolling at least a pixel
      when (Math.abs x >= one) do
        trackDims <- _.padded <<< browserSlots <$> getDimensions bc
        browser.queueUpdateView
         $ ScrollView $ (-x) / trackDims.size.width

    let scrollZoomScale = 0.06
    wheelZoom bc \dY ->
       browser.queueUpdateView
         $ ZoomView $ 1.0 + scrollZoomScale * dY


  liftEffect do
    let sigSnps = filterSig config.score trackData.snps
        annotAround pks snp =
          Array.find (\a -> a.covers `pairsOverlap` snp.position)
            =<< Map.lookup snp.feature.chrId pks

        glyphClick :: Point -> Effect Unit
        glyphClick p = launchAff_ do
          v  <- browser.getView

          trackDims <- _.padded <<< browserSlots <$> getDimensions bc
          let segs = pixelSegments { segmentPadding: 12.0 } cSys trackDims.size v
              annoPeaks = annotationsForScale cSys sigSnps
                            trackData.annotations segs

          lastHotspots' <- liftEffect $ browser.lastHotspots
          liftEffect do
            let clicked = (lastHotspots' clickRadius p)
            printSNPInfo clicked
            case Array.head clicked of
              Nothing -> cmdInfoBox IBoxHide
              Just g  -> do
                cmdInfoBox IBoxShow
                cmdInfoBox $ IBoxSetX $ Int.round p.x
                cmdInfoBox $ IBoxSetY $ Int.round p.y
                cmdInfoBox $ IBoxSetContents
                  $ snpHTML g
                  <> foldMap annoPeakHTML (annotAround annoPeaks g)


    browserClickHandler bc
      $ Padded 5.0 glyphClick

  pure unit



type DataURLs = { snps        :: Maybe String
                , annotations :: Maybe String
                , genes       :: Maybe String
                }



type Conf =
  { browserHeight :: Number
  , trackPadding :: TrackPadding
  , score :: { min :: Number, max :: Number, sig :: Number }
  , urls :: DataURLs
  , chrLabels :: { fontSize :: Int }
  , snpsConfig        :: SNPConfig
  , annotationsConfig :: AnnotationsConfig
  , legend :: LegendConfig ()
  , vscale :: VScale ()
  , initialChrs :: Maybe { left :: String, right :: String }
  }

foreign import setWindow :: ∀ a. String -> a -> Effect Unit


main :: Foreign -> Effect Unit
main rawConfig = do

  el' <- do
    doc <- DOM.toDocument
           <$> (DOM.document =<< DOM.window)
    DOM.querySelector (wrap "#browser") (toParentNode doc)


  case el' of
    Nothing -> log "Could not find element '#browser'"
    Just el -> do

      case read rawConfig :: Either MultipleErrors Conf of
        Left errs -> do
          setElementContents el
            $  "<p>Error when parsing provided config object:<p>"
            <> foldMap (wrapWith "p" <<< renderForeignError) errs

        Right c   -> do

              {width} <- windowInnerSize
              let dimensions = { width, height: c.browserHeight }
              bc <- browserContainer dimensions c.trackPadding el

              log $ unsafeStringify c
              void $ runBrowser c bc



-- TODO this could almost certainly be done better
-- chunkConsumer :: ∀ m a.
--                  Foldable m
--               => Monoid (m a)
--               => AVar (m a)
--               -> Consumer (m a) (Aff) Unit
-- chunkConsumer av = Co.consumer \m ->
--   if null m
--     then pure $ Just unit
--     else do
--       sofar <- AVar.take av
--       AVar.put (sofar <> m) av
--       delay (wrap 20.0)
--       pure Nothing


-- type TrackVar a = AVar (Map ChrId (Array a))
-- type TrackProducer eff a = Producer (Map ChrId (Array a)) (Aff eff) Unit

-- Feels like this one takes care of a bit too much...
-- fetchLoop1 :: ∀ a.
--               (Maybe (Aff (TrackProducer _ a)))
--            -> Aff (TrackVar a)
-- fetchLoop1 Nothing = AVar.empty
-- fetchLoop1 (Just startProd) = do
--   prod <- startProd
--   avar <- AVar.new mempty
--   _ <- forkAff $ runProcess $ prod `connect` chunkConsumer avar
--   pure avar

-- | Starts threads that fetch & parse each of the provided tracks,
-- | filling an AVar over time per track, which can be used by other parts of the application
-- | (read only, should be a newtype)
{-
fetchLoop :: CoordSys ChrId BigInt
          -> DataURLs
          -> Aff
               { gwas :: TrackVar (GWASFeature ())
               , genes :: TrackVar BedFeature
               }
fetchLoop cs urls = do
  gwas <-        fetchLoop1 $ produceGWAS   cs <$> urls.gwas
  genes <-       fetchLoop1 $ produceGenes  cs <$> urls.genes
  pure { gwas, genes }
-}


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
      ]
