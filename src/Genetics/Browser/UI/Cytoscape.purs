module Genetics.Browser.UI.Cytoscape
       where

import Prelude
import Control.Coroutine as CR
import Genetics.Browser.Cytoscape as Cytoscape
import Genetics.Browser.Events as GBE
import Genetics.Browser.Feature.Foreign as FF
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as Affjax
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Genetics.Browser.Cytoscape (CyCollection, CyElement, ParsedEvent(..), resize, runLayout)
import Genetics.Browser.Events (eventLocation, eventRange, eventScore)
import Genetics.Browser.Events.Types (Event, EventLocation(..), EventRange(..))
import Genetics.Browser.Types (CY, Cytoscape)
import Genetics.Browser.Units (Bp(..))
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX)

parseEvent :: JObject -> Either String Event
parseEvent = FF.parseFeatureLocation' { locKeys: ["lrsLoc"]
                                      , chrKeys: ["chr"]
                                      , posKeys: ["pos"]
                                      }

{-
Create callback, subscriber event source like before (like already exists?)
In the respective query, parse the event? Or do that before.
  Cleaner if done in the query, but might be more difficult to generalize,
  since a generic track won't be able to use them.
    Doesn't actually matter now! these tracks are different types.

So, send ParsedEvent to Query, then parse to Event... These names are awful
Raise SendEvent msg with Event
In Main, send to other track, as a Query (RecvEvent).
In BD, try to parse received event and act accordingly.
-}



-- TODO: elemsUrl should be safer. Maybe it should cache too, idk
type State = { cy :: Maybe Cytoscape
             , elemsUrl :: String
             }

data Query a
  = Initialize String a
  | Reset a
  | Filter (JObject -> Boolean) a
  | RecvEvent Event a
  | RaiseEvent Cytoscape.ParsedEvent (H.SubscribeStatus -> a)

data Output
  = SendEvent Event

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

  getElements :: forall eff'. String -> Aff (ajax :: AJAX | eff') (CyCollection CyElement)
  getElements url = Affjax.get url <#> (\r -> Cytoscape.unsafeParseCollection r.response)

  getAndSetElements :: forall eff'. String -> Cytoscape -> Aff (ajax :: AJAX, cy :: CY | eff') Unit
  getAndSetElements url cy = do
    eles <- getElements url
    liftEff $ Cytoscape.coreAddCollection cy eles

  eval :: Query ~> H.ComponentDSL State Query Output (Aff (Effects eff))
  eval = case _ of
    Initialize url next -> do
      H.getHTMLElementRef (H.RefLabel "cy") >>= case _ of
        Nothing -> pure unit
        Just el' -> do
          cy <- liftEff $ Cytoscape.cytoscape el' Nothing

          liftAff $ getAndSetElements url cy

          liftEff $ do
            runLayout cy Cytoscape.circle
            resize cy

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
              liftEff $ Cytoscape.coreRemoveAllElements cy
              -- refetch all elements
              liftAff $ getAndSetElements url cy

              liftEff $ log $ "resetting with stored URL " <> url
              pure unit

          liftEff $ do
            runLayout cy Cytoscape.circle
            resize cy
      pure next


    Filter pred next -> do
      H.gets _.cy >>= case _ of
        Nothing -> pure unit
        Just cy -> do
          eles <- liftEff $ Cytoscape.coreFilterImpl cy pred
          _ <- liftEff $ Cytoscape.collRemoveElements eles
          pure unit
      pure next


    RaiseEvent (ParsedEvent pev) reply -> do

      case pev.target of
          Left el -> do
            d <- liftEff $ Cytoscape.eleGetAllData el
            case parseEvent d of
              Left err  -> pure unit
              Right loc ->
                H.raise $ SendEvent loc

          Right cy -> pure unit

      pure $ reply H.Listening


    RecvEvent ev next -> do

      H.gets _.cy >>= case _ of
        Nothing -> pure next
        Just cy -> do

          case eventLocation ev of
            Left err -> pure unit
            Right (EventLocation l) -> ?filterLoc

          case eventRange ev of
            Left err -> pure unit
            Right (EventRange r) -> ?filterRange

          case eventScore ev of
            Left err -> pure unit
            Right (EventScore s) -> ?filterScore

          pure next
