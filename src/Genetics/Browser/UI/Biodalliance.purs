module Genetics.Browser.UI.Biodalliance
       where

import Prelude
import Genetics.Browser.Events.Types
import Genetics.Browser.Biodalliance as Biodalliance
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Genetics.Browser.Events (eventLocation, eventRange, eventScore)
import Genetics.Browser.Types (BD, Biodalliance)
import Genetics.Browser.Units (Bp)


type State = { bd :: Maybe Biodalliance
             }

data Query a
  = Scroll Bp a
  | Jump String Bp Bp a
  | Initialize (forall eff. HTMLElement -> Eff eff Biodalliance) a
  | InitializeCallback (H.SubscribeStatus -> a)
  | Click JObject (H.SubscribeStatus -> a)
  | RecvEvent Event a

data Message
  = Initialized
  | Clicked JObject

type Effects eff = (avar :: AVAR, bd :: BD | eff)

data Slot = Slot
derive instance eqBDSlot :: Eq Slot
derive instance ordBDSlot :: Ord Slot

component :: ∀ eff. H.Component HH.HTML Query Unit Message (Aff (Effects eff))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { bd: Nothing }

  render :: State -> H.ComponentHTML Query
  render = const $ HH.div [ HP.ref (H.RefLabel "bd")
                          , HP.id_ "bdDiv"
                          ] []

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (Effects eff))
  eval = case _ of
    Initialize mkBd next -> do
      H.getHTMLElementRef (H.RefLabel "bd") >>= case _ of
        Nothing -> pure unit
        Just el -> do
          bd <- liftEff $ mkBd el
          H.subscribe $ H.eventSource_ (Biodalliance.onInit bd) (H.request InitializeCallback)
          H.subscribe $ H.eventSource (Biodalliance.addFeatureListener bd) $ Just <<< H.request <<< Click

          H.modify (_ { bd = Just bd })
      pure next

    Scroll n next -> do
      mbd <- H.gets _.bd
      case mbd of
        Nothing -> pure next
        Just bd -> do
          liftEff $ Biodalliance.scrollView bd n
          pure next

    Jump chr xl xr next -> do
      mbd <- H.gets _.bd
      case mbd of
        Nothing -> pure next
        Just bd -> do
          liftEff $ Biodalliance.setLocation bd chr xl xr
          pure next

    InitializeCallback reply -> do
      H.raise $ Initialized
      pure $ (reply H.Listening)

    Click obj reply -> do
      H.raise $ Clicked obj
      pure $ reply H.Listening
    RecvEvent ev next -> do

      case eventLocation ev of
        Left err -> pure unit
        Right l  -> ?scrollL
      case eventRange ev of
        Left err -> pure unit
        Right r  -> ?scrollR
      case eventScore ev of
        Left err -> pure unit
        Right s  -> ?dealWithScore

      pure next
