module Genetics.Browser.UI.Biodalliance
       where

import Prelude
import Genetics.Browser.Biodalliance as Biodalliance
import Genetics.Browser.Feature.Foreign as FF
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut (Json, _Number, _String)
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.Lens (re, (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Genetics.Browser.Events (EventLocation(..), EventRange(..), parseLocation, parseRange)
import Genetics.Browser.Events.Types (Event(..))
import Genetics.Browser.Types (BD, Biodalliance)
import Genetics.Browser.Units (Bp, Chr(..), MBp(..), bp, mbp)
import Global.Unsafe (unsafeStringify)


type State = { bd :: Maybe Biodalliance
             }

data Query a
  = Scroll Bp a
  | Jump Chr Bp Bp a
  | Initialize (forall eff. HTMLElement -> Eff eff Biodalliance) a
  | InitializeCallback (H.SubscribeStatus -> a)
  | RaiseEvent JObject (H.SubscribeStatus -> a)
  | RecvEvent Event a

data Message
  = Initialized
  | SendEvent Event

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
            $ Just <<< H.request <<< RaiseEvent

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


    RaiseEvent ft reply -> do
      let chr = ft ^? ix "chr" <<< _String <<< re _String
          minPos = ft ^? ix "min" <<< _Number <<< re _Number
          maxPos = ft ^? ix "max" <<< _Number <<< re _Number
          obj = traverse sequence [Tuple "chr" chr, Tuple "minPos" minPos, Tuple "maxPos" maxPos]
      case obj of
        Nothing -> liftEff $ log "Error when parsing chr, min, max"
        Just o  -> H.raise $ SendEvent $ Event $ fromFoldable o

      pure $ reply H.Listening


    RecvEvent ev next -> do
      mbd <- H.gets _.bd
      case mbd of
        Nothing -> pure next
        Just bd -> do

          case parseLocation ev of
            Nothing  -> liftEff $ log "couldn't parse location from event"
            Just (EventLocation loc) -> do
              liftEff $ log $ unsafeStringify loc
              let minPos = loc.pos - bp (MBp 1.5)
                  maxPos = loc.pos + bp (MBp 1.5)
              liftEff $ Biodalliance.setLocation bd loc.chr (mbp minPos) (mbp maxPos)

          case parseRange ev of
            Nothing  -> liftEff $ log "couldn't parse range from event"
            Just (EventRange r) -> do
              liftEff $ Biodalliance.setLocation bd r.chr r.minPos r.maxPos

          -- case eventScore ev of
          --   Left err -> pure unit
          --   Right (EventScore s) -> pure unit


          pure next
