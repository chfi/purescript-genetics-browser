module Genetics.Browser.UI.Cytoscape
       where

import Prelude
import Genetics.Browser.Cytoscape as Cytoscape
import Genetics.Browser.Feature.Foreign as FF
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as Affjax
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Argonaut (Json, _JsonNumber, _JsonString, _Number, _Object, _String)
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.Exists (Exists)
import Data.Foreign (F)
import Data.Foreign.Class (decode, encode)
import Data.Lens (re, (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.StrMap (fromFoldable)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Genetics.Browser.Cytoscape (ParsedEvent(..), runLayout, resizeContainer)
import Genetics.Browser.Cytoscape.Collection (CyCollection, connectedNodes, filter, isEdge, union)
import Genetics.Browser.Cytoscape.Types (CY, Cytoscape, Element, elementJObject, elementJson)
import Genetics.Browser.Events (EventLocation(..), EventRange(..), JsonEvent(..))
import Genetics.Browser.Feature.Foreign (deepObjIx, parsePath)
import Genetics.Browser.Units (_Bp, _BpMBp, _Chr, _MBp, bp)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX)
import Unsafe.Coerce (unsafeCoerce)


-- TODO: elemsUrl should be safer. Maybe it should cache too, idk
type State = { cy :: Maybe Cytoscape
             , elemsUrl :: String
             }

data Query a
  = Initialize String a
  | Reset a
  | Filter (Element -> Boolean) a
  | RecvEvent JsonEvent a
  | RaiseEvent Cytoscape.ParsedEvent (H.SubscribeStatus -> a)

data Output
  = SendEvent JsonEvent

type Effects eff = ( cy :: CY
                   , ajax :: AJAX
                   , console :: CONSOLE
                   , exception :: EXCEPTION
                   , avar :: AVAR | eff)

data Slot = Slot
derive instance eqCySlot :: Eq Slot
derive instance ordCySlot :: Ord Slot

component :: ∀ eff. H.Component HH.HTML Query Unit Output (Aff (Effects eff))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { cy: Nothing
                 , elemsUrl: ""
                 }

  -- TODO: set css here instead of pgb.html
  render :: State -> H.ComponentHTML Query
  render = const $ HH.div [ HP.ref (H.RefLabel "cy")
                          , HP.id_ "cyDiv"
                          -- , HP.prop
                          ] []


  -- for some reason having an explicit forall makes the rest of the file not get parsed by purs-ide...
  -- getElements :: ∀ eff'. String -> Aff (ajax :: AJAX | eff') CyCollection
  getElements :: _
  getElements url = Affjax.get url <#> (\r -> Cytoscape.unsafeParseCollection r.response)

  -- getAndSetElements :: ∀ eff'. String -> Cytoscape -> Aff (ajax :: AJAX, cy :: CY | eff') Unit
  getAndSetElements :: _
  getAndSetElements url cy = do
    eles <- getElements url
    liftEff $ Cytoscape.graphAddCollection cy eles


  eval :: Query ~> H.ComponentDSL State Query Output (Aff (Effects eff))
  eval = case _ of
    Initialize url next -> do
      H.getHTMLElementRef (H.RefLabel "cy") >>= case _ of
        Nothing -> pure unit
        Just el' -> do
          cy <- liftEff $ Cytoscape.cytoscape (Just el') Nothing

          liftAff $ getAndSetElements url cy

          liftEff $ do
            runLayout cy Cytoscape.circle
            resizeContainer cy

          H.subscribe $ H.eventSource (Cytoscape.onClick cy) $ Just <<< H.request <<< RaiseEvent
          H.modify (_ { cy = Just cy
                      , elemsUrl = url
                      })
      pure next


    Reset next -> do
      H.gets _.cy >>= case _ of
        Nothing -> (liftEff $ log "No cytoscape found!.") *> pure unit
        Just cy -> do
          H.gets _.elemsUrl >>= case _ of
            "" -> do
              liftEff $ log "no element URL; can't reset"
              pure unit
            url -> do
              -- remove all elements
              liftEff $ do
                Cytoscape.graphRemoveAll cy
                log $ "resetting with stored URL " <> url

              -- refetch & set all elements
              liftAff $ getAndSetElements url cy

              pure unit

          liftEff $ do
            runLayout cy Cytoscape.circle
            resizeContainer cy

      pure next


    Filter pred next -> do
      H.gets _.cy >>= case _ of
        Nothing -> pure unit
        Just cy -> do
          graphColl <- liftEff $ Cytoscape.graphGetCollection cy
          let eles = filter pred graphColl
          _ <- liftEff $ Cytoscape.graphRemoveCollection eles
          pure unit

      pure next


    RaiseEvent (ParsedEvent pev) reply -> do

      case pev.target of
          Left el -> do
            case cyParseEventLocation el of
              Nothing -> liftEff $ log "Error when parsing chr, pos of cytoscape event"
              Just obj' -> H.raise $ SendEvent $ JsonEvent $ encode obj'

          Right cy -> pure unit

      pure $ reply H.Listening


    RecvEvent (JsonEvent ev) next -> do

      H.gets _.cy >>= case _ of
        Nothing -> pure next
        Just cy -> do
          liftEff $ log "received event"
          liftEff $ log $ unsafeStringify ev

          case runExcept $ decode ev :: F EventRange of
            Left _  -> liftEff $ log "couldn't parse range from event"
            Right (EventRange ran) -> do
              liftEff $ log $ unsafeStringify ran
              let pred el = case cyParseEventLocation el of
                    Nothing -> false
                    Just (EventLocation loc) -> loc.chr /= ran.chr

              graphColl <- liftEff $ Cytoscape.graphGetCollection cy
              let eles = filter pred graphColl
              _ <- liftEff $ Cytoscape.graphRemoveCollection eles
              pure unit


          pure next


-- TODO this should be less ad-hoc, somehow. future probs~~~
cyParseEventLocation :: Element -> Maybe EventLocation
cyParseEventLocation el = case (elementJson el) ^? FF.deepObjIx ["data", "lrsLoc"] of
  Nothing  -> Nothing
  Just loc -> do
    chr <- loc ^? _Object <<< ix "chr" <<< _String <<< re _Chr
           -- ridiculous.
    pos <- loc ^? _Object <<< ix "pos" <<< _Number
                  <<< re _MBp <<< re _BpMBp
    pure $ EventLocation { chr, pos }
