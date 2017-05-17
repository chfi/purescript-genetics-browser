module Genetics.Browser.UI.Cytoscape
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
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (type (<\/>), Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Nullable (toNullable)
import Genetics.Browser.Cytoscape (ParsedEvent(..), ajaxAddEles, cytoscape, resize, runLayout)
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Source.QTL (fetch)
import Genetics.Browser.Types (BD, Biodalliance, CY, Cytoscape, Renderer)
import Genetics.Browser.Units (Bp(..))
import Global.Unsafe (unsafeStringify)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)


-- TODO: elemsUrl should be safer. Maybe it should cache too, idk
type State = { cy :: Maybe Cytoscape
             , elemsUrl :: String
             }

data Query a
  = Initialize String a
  | Reset a
  | Filter (Cytoscape.CyElement -> Boolean) a
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

  eval :: Query ~> H.ComponentDSL State Query Output (Aff (Effects eff))
  eval = case _ of
    Initialize url next -> do
      H.getHTMLElementRef (H.RefLabel "cy") >>= case _ of
        Nothing -> pure unit
        Just el' -> do
          -- TODO: figure out Aff. right now it just runs straight through, running the layout before elements have loaded
          cy <- liftEff $ Cytoscape.cytoscapeImpl el' (toNullable Nothing)
          liftEff $ log "Fetching elements"
          eles <- liftAff $ ajaxAddEles cy url
          liftEff $ log "Adding elements"
          _ <- liftEff $ Cytoscape.cyAdd cy eles
          liftEff $ log "Elements added"
          -- H.subscribe $ H.eventSource (Cytoscape.onClick cy) $ H.request $ const (Just H.Listening)
          H.subscribe $ H.eventSource (Cytoscape.onClick cy) $ Just <<< H.request <<< Click

          liftEff $ do
            runLayout cy Cytoscape.circle
            resize cy
            Cytoscape.onClick cy $ \(ParsedEvent ev) -> case ev.target of
              Left el -> log "Clicked element!"
              Right _ -> log "Clicked cy?!"

          H.modify (_ { cy = Just cy
                      , elemsUrl = url
                      })
          -- s <- H.get
          -- liftEff $ log $ unsafeStringify s
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
              -- refetch all elements
              liftEff $ log $ "resetting with stored URL " <> url
              pure unit

          liftEff $ do
            runLayout cy Cytoscape.circle
            resize cy
            Cytoscape.onClick cy $ \_ -> log "what's up"
      pure next
    Filter pred next -> do
      pure next
    Click el reply -> do
      H.raise $ Clicked el
      pure $ reply H.Listening
