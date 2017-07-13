module Genetics.Browser.UI.Biodalliance
       where

import Prelude
import Genetics.Browser.Biodalliance as Biodalliance
import Genetics.Browser.UI.Events.Biodalliance as BDEvents
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut (_Number, _String)
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign.Class (decode, encode)
import Data.Lens (re, (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, default, inj, on)
import Genetics.Browser.Events (Event(..), EventLocation, EventRange, Location, Range, _eventLocation, _eventRange, handleLocation, handleRange)
import Genetics.Browser.Types (BD, Biodalliance)
import Genetics.Browser.Units (Bp, Chr, MBp(MBp), _Bp, _Chr, bp, mbp)
import Global.Unsafe (unsafeStringify)
import Unsafe.Coerce (unsafeCoerce)


type State = { bd :: Maybe Biodalliance
             }

data Query a
  = Scroll Bp a
  | Jump Chr Bp Bp a
  | Initialize (forall eff. HTMLElement -> Eff (bd :: BD | eff) Biodalliance) a
  | InitializeCallback (H.SubscribeStatus -> a)
  | EventFromBD JObject (H.SubscribeStatus -> a)

data Message
  = Initialized
  | SendBD Biodalliance

type Effects eff = (avar :: AVAR, bd :: BD, console :: CONSOLE | eff)

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

          H.subscribe $ H.eventSource (Biodalliance.addFeatureListener bd)
            $ Just <<< H.request <<< EventFromBD
          H.raise $ SendBD bd

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
      pure $ reply H.Listening


    EventFromBD ft reply -> do


      pure $ reply H.Listening
