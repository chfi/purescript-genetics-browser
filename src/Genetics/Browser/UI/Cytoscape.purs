module Genetics.Browser.UI.Cytoscape
       where

import Prelude
import Genetics.Browser.Cytoscape as Cytoscape
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
import Data.Maybe (Maybe(..))
import Genetics.Browser.Cytoscape (CyCollection, CyElement, resize, runLayout)
import Genetics.Browser.Types (CY, Cytoscape)
import Network.HTTP.Affjax (AJAX)


-- TODO: elemsUrl should be safer. Maybe it should cache too, idk
type State = { cy :: Maybe Cytoscape
             , elemsUrl :: String
             }

data Query a
  = Initialize String a
  | Reset a
  | Filter (JObject -> Boolean) a
  | Click Cytoscape.ParsedEvent (H.SubscribeStatus -> a)

data Output
  = Clicked Cytoscape.ParsedEvent

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

          H.subscribe $ H.eventSource (Cytoscape.onClick cy) $ Just <<< H.request <<< Click
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

    Click el reply -> do
      H.raise $ Clicked el
      pure $ reply H.Listening
