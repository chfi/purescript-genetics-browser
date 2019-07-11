module Genetics.Browser.UI where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (Free)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left))
import Data.Filterable (filterMap)
import Data.Foldable (foldMap, length, sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Lens ((^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Pair (Pair(..))
import Data.Pair as Pair
import Data.Symbol (SProxy(SProxy))
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(Tuple))
import Data.Variant (Variant, case_, inj)
import Data.Variant as V
import Effect (Effect)
import Effect.Aff (Aff, Fiber, forkAff, killFiber, launchAff, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error, throw)
import Effect.Ref as Ref
import Foreign (Foreign, MultipleErrors, renderForeignError)
import Genetics.Browser (HexColor, Peak, pixelSegments)
import Genetics.Browser.Bed (getGenes)
import Genetics.Browser.Canvas (setTextContent, BrowserContainer, TrackAnimation(..), TrackContainer, _Container, addTrack, animateTrack, browserContainer, dragScroll, dragScrollTrack, getDimensions, getTrack, setElementStyle, setTrackContainerSize, trackClickHandler, wheelZoom, withLoadingIndicator)
import Genetics.Browser.Coordinates (CoordSys, CoordSysView(..), _Segments, _TotalSize, coordSys, normalizeView, pairsOverlap, scaleToScreen, viewScale)
import Genetics.Browser.Demo (Annotation, AnnotationField, SNP, addChrLayers, addGWASLayers, addGeneLayers, annotationsForScale, filterSig, getAnnotations, getSNPs, showAnnotationField)
import Genetics.Browser.Layer (Component(Center), trackSlots)
import Genetics.Browser.Cacher as Cacher
import Genetics.Browser.Track (class TrackRecord, makeContainers, makeTrack)
import Genetics.Browser.Types (ChrId(ChrId), _NegLog10, _prec)
import Genetics.Browser.UI.View (UpdateView(..))
import Genetics.Browser.UI.View as View
import Global.Unsafe (unsafeStringify)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Point)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Prim.RowList (class RowToList)
import Record as Record
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



{-
track commands/script:

1. get the track container from the browser container by name
2. set the loading indicator in a track container
3. fetch datasets in Aff
4. do something with the fetched datasets (general callback, possibly applied to trackcontainer)
5. compile a given set of layers into a render function
6. initialize a track with a render function + coordsys + track container
7. add callbacks using Effect and a given track container's TrackInterface
8. add callbacks using Effect, TrackInterface, arbitrary set of config & data (derived config)
9. updating/showing the infobox (essentially just Effect Unit)


the tricky part is really representing the different types of the
datasets; since there can only be *one* type in the BrowserCmdF,
"additional" types must be either in the `a` or, and this is kinda
ugly but might make sense, they are provided by the `BrowserState`,
when interpreting it into `Aff a` using `cmdAff`.

Makes sense -- `Aff` can produce the required data, while
`BrowserState` provides another set of data points...

-}


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


type UICmdR = ( render :: Unit
              , docResize :: { width :: Number, height :: Number } )

_render = SProxy :: SProxy "render"
_docResize = SProxy :: SProxy "docResize"

-- | The value returned by the function that initializes the genome browser;
-- | provides an interface to the running track instance.
type TrackInterface a
  = { lastHotspots :: Aff (Number -> Point -> Array a)
    , queueCommand :: Variant UICmdR -> Aff Unit
    }


-- | Creates the track using the provided initial data, returning
-- | a TrackInterface for reading state & sending commands to it
initializeTrack :: ∀ c r rl a.
                   RowToList r rl
                => TrackRecord rl r a
                => CoordSys c BigInt
                -> Record r
                -> Effect CoordSysView
                -> TrackContainer
                -> Aff (TrackInterface a)
initializeTrack cSys renderFuns getView tc = do

  renderFiberVar <- AVar.empty
  uiCmdVar <- AVar.empty

  track <- makeTrack renderFuns tc

  let queueCommand = flip AVar.put uiCmdVar

      mainLoop = do
        uiCmd <- AVar.take uiCmdVar

        V.match { render: \_ -> pure unit

                , docResize: \ {width} -> do
                    {height} <- _.size <$> getDimensions tc
                    setTrackContainerSize {width, height} tc
                    queueCommand $ inj _render unit
                    mainLoop

                } uiCmd


        -- if there's a rendering fiber running, we kill it
        traverse_ (killFiber (error "Resetting renderer"))
          =<< AVar.tryTake renderFiberVar

        csView <- liftEffect $ getView

        currentDims <- getDimensions tc

        let trackDims = _.center $ trackSlots currentDims
            currentScale = viewScale trackDims.size csView
            pxView = scaleToScreen currentScale <$> (unwrap csView)

        -- fork a new renderFiber
        renderFiber <- forkAff
                       $ track.render pxView csView

        AVar.put renderFiber renderFiberVar

        mainLoop

  _ <- forkAff mainLoop

  queueCommand $ inj _render unit

  pure { lastHotspots: track.hotspots
       , queueCommand }


btnUI :: { scrollMod :: Number, zoomMod :: Number }
      -> (UpdateView -> Effect Unit)
      -> Effect Unit
btnUI mods cb = do
  buttonEvent "scrollLeft"  $ cb $ ScrollView     (-mods.scrollMod)
  buttonEvent "scrollRight" $ cb $ ScrollView       mods.scrollMod
  buttonEvent "zoomOut"     $ cb $ ZoomView $ 1.0 + mods.zoomMod
  buttonEvent "zoomIn"      $ cb $ ZoomView $ 1.0 - mods.zoomMod


btnUIFixed :: (UpdateView -> Effect Unit)
           -> Effect Unit
btnUIFixed = btnUI { scrollMod: 0.5, zoomMod: 1.0 }

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

updateInfoBox :: _
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


runBrowser :: BrowserConfig { gwas :: _
                            , gene :: _ }
           -> BrowserContainer
           -> Effect (Fiber Unit)
runBrowser config bc = launchAff $ do

  let cSys :: CoordSys ChrId BigInt
      cSys = coordSys mouseChrSizes
      initialView = fromMaybe (wrap $ Pair zero (cSys^._TotalSize)) do
        v <- config.initialChrs
        (Pair l _) <- Map.lookup (wrap v.left) $ cSys^._Segments
        (Pair _ r) <- Map.lookup (wrap v.right) $ cSys^._Segments
        pure $ wrap $ Pair l r

      clickRadius = 1.0

  liftEffect $ initDebugDiv clickRadius

  cmdInfoBox <- liftEffect $ initInfoBox


  viewManager <- liftEffect $ View.browserViewManager
                 cSys
                 (wrap 200.0)
                 { initialView }
                 bc

  -- Here *global* callbacks/input handlers are set, i.e. buttons &
  -- keys that don't depend on track canvases
  liftEffect do
    let btnMods = { scrollMod: 0.10, zoomMod: 0.15 }
    btnUI btnMods \u ->
      viewManager.updateView u

    buttonEvent "reset" do
      let cmd = ModView (const $ unwrap initialView)
      viewManager.updateView cmd

    keyUI (bc ^. _Container) { scrollMod: 0.075 } \u -> do
      viewManager.updateView u

    let scrollZoomScale = 0.06
    wheelZoom bc \dY -> do
      let cmd = ZoomView $ 1.0 + scrollZoomScale * dY
      viewManager.updateView cmd



  -- this is used to set callbacks/input handlers for track/track
  -- canvas-dependent inputs
  let setHandlers tc track = liftEffect do

        resizeEvent \d -> do
          launchAff_ $ track.queueCommand $ inj _docResize d

        dragScrollTrack tc \ {x,y} -> do
          -- only do anything when scrolling at least a pixel
          when (Math.abs x >= one) do
            trackDims <- _.center <<< trackSlots <$> getDimensions tc
            let cmd = ScrollView $ (x) / trackDims.size.width
            viewManager.updateView cmd



  gwasTrack <- forkAff do

    gwasTC <- getTrack "gwas" bc
    gwasData <- withLoadingIndicator gwasTC
                $  {snps:_, annotations:_}
               <$> foldMap (getSNPs        cSys) config.urls.snps
               <*> foldMap (getAnnotations cSys) config.urls.annotations
    render <- do
      chrLayers <- addChrLayers { coordinateSystem: cSys
                                , segmentPadding: 12.0 }
                                config.chrs gwasTC
      gwasLayers <- addGWASLayers cSys config.tracks.gwas gwasData gwasTC
      pure $ Record.merge { chrs: chrLayers } gwasLayers

    track <- initializeTrack cSys render viewManager.browserView gwasTC
    setHandlers gwasTC track

    liftEffect $
      viewManager.addCallback \csv ->
        launchAff_ $ track.queueCommand (inj _render unit)

    liftEffect do
      let sigSnps = filterSig config.score gwasData.snps
          annotAround pks snp =
            Array.find (\a -> a.covers `pairsOverlap` snp.position)
              =<< Map.lookup snp.feature.chrId pks

          glyphClick :: Point -> Effect Unit
          glyphClick p = launchAff_ do
            v <- liftEffect $ viewManager.browserView

            trackDims <- _.center <<< trackSlots <$> getDimensions gwasTC
            let segs = pixelSegments { segmentPadding: 12.0 } cSys trackDims.size v
                annoPeaks = annotationsForScale cSys sigSnps
                              gwasData.annotations segs


            lastHotspots' <- track.lastHotspots
            let clicked = lastHotspots' clickRadius p

            liftEffect do
              case Array.head clicked of
                Nothing -> cmdInfoBox IBoxHide
                Just g  -> do
                  cmdInfoBox IBoxShow
                  cmdInfoBox $ IBoxSetX $ Int.round p.x
                  cmdInfoBox $ IBoxSetY $ Int.round p.y
                  cmdInfoBox $ IBoxSetContents
                    $ snpHTML g
                    <> foldMap annoPeakHTML (annotAround annoPeaks g)


      trackClickHandler gwasTC
        $ Center glyphClick


  geneTrack <- forkAff do

    geneTC <- getTrack "gene" bc

    genes <- withLoadingIndicator geneTC case config.urls.genes of
        Nothing  -> throwError $ error "no genes configured"
        Just url -> do
          log $ "fetching genes"
          g <- getGenes cSys url
          log $ "genes fetched: " <> show (sum $ Array.length <$> g)
          pure g

    render <- do
      chrLayers <- addChrLayers { coordinateSystem: cSys
                                , segmentPadding: 12.0 }
                                config.chrs geneTC
      geneLayers <- addGeneLayers cSys config.tracks.gene { genes } geneTC

      pure $ Record.merge { chrs: chrLayers } geneLayers

    track <- initializeTrack cSys render viewManager.browserView geneTC
    setHandlers geneTC track


    liftEffect $ viewManager.addCallback \csv ->
        launchAff_ $ track.queueCommand (inj _render unit)


  pure unit



type DataURLs = { snps        :: Maybe String
                , annotations :: Maybe String
                , genes       :: Maybe String
                }


type BrowserConfig a =
  { score :: { min :: Number, max :: Number, sig :: Number }
  , urls :: DataURLs
  , initialChrs :: Maybe { left :: String, right :: String }
  , chrs ::  { chrLabels :: { fontSize :: Int }
             , chrBG1 :: HexColor
             , chrBG2 :: HexColor }
  , tracks :: a
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

      case read rawConfig :: Either MultipleErrors (BrowserConfig _) of
        Left errs -> do
          setElementContents el
            $  "<p>Error when parsing provided config object:<p>"
            <> foldMap (wrapWith "p" <<< renderForeignError) errs

        Right c   -> do

          {width} <- windowInnerSize

          cs <- makeContainers width c.tracks

          bc <- browserContainer el

          addTrack bc "gwas" cs.gwas
          addTrack bc "gene" cs.gene

          log $ unsafeStringify c
          void $ runBrowser c bc


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
