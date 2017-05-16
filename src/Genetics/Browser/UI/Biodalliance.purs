module Genetics.Browser.UI.Biodalliance
       where

import Prelude
import Genetics.Browser.Biodalliance as Biodalliance
import Genetics.Browser.Cytoscape as Cytoscape
import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot as QTL
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import DOM.HTML.Types (HTMLElement)
import Data.Const (Const(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (type (<\/>), Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Source.QTL (fetch)
import Genetics.Browser.Types (BD, Biodalliance, CY, Cytoscape, Renderer)
import Genetics.Browser.Units (Bp(..))
import Global.Unsafe (unsafeStringify)
import Halogen.VDom.Driver (runUI)


type State = { bd :: Maybe Biodalliance }

data Query a
  = Scroll Number a
  | Jump String Number Number a
  | Initialize (forall eff. HTMLElement -> Eff eff Biodalliance) a
  | InitializeCallback (H.SubscribeStatus -> a)

data Message = Initialized

type BDEffects eff = (avar :: AVAR, bd :: BD | eff)

component :: ∀ eff. H.Component HH.HTML Query Unit Message (Aff (BDEffects eff))
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

  -- doesn't actually render anything...
  render :: State -> H.ComponentHTML Query
  render = const $ HH.div [ HP.ref (H.RefLabel "bd")
                          , HP.id_ "svgHolder"
                          ] []

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (BDEffects eff))
  -- eval :: AceQuery ~> H.ComponentDSL AceState AceQuery AceOutput (Aff (AceEffects eff))
  eval = case _ of
    Initialize mkBd next -> do
      H.getHTMLElementRef (H.RefLabel "bd") >>= case _ of
        Nothing -> pure unit
        Just el -> do
          bd <- liftEff $ mkBd el
          H.subscribe $ H.eventSource_ (Biodalliance.onInit bd) (H.request InitializeCallback)
          H.modify (_ { bd = Just bd })
      pure next
    Scroll n next -> do
      mbd <- H.gets _.bd
      case mbd of
        Nothing -> pure next
        Just bd -> do
          liftEff $ Biodalliance.scrollView bd (Bp n)
          pure next
    Jump chr xl xr next -> do
      mbd <- H.gets _.bd
      case mbd of
        Nothing -> pure next
        Just bd -> do
          liftEff $ Biodalliance.setLocation bd chr (Bp xl) (Bp xr)
          pure next
    InitializeCallback reply -> do
      H.raise $ Initialized
      pure $ (reply H.Listening)


data Slot = Slot
derive instance eqBDSlot :: Eq Slot
derive instance ordBDSlot :: Ord Slot
