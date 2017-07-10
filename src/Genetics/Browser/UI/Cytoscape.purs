module Genetics.Browser.UI.Cytoscape
       where

import Prelude
import Genetics.Browser.Cytoscape as Cytoscape
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
import Data.Argonaut (_Number, _Object, _String)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign.Class (decode, encode)
import Data.Lens (re, (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Predicate (Predicate(..))
import Data.Variant (default, inj)
import Genetics.Browser.Cytoscape (ParsedEvent(..), runLayout, resizeContainer)
import Genetics.Browser.Cytoscape.Collection (filter)
import Genetics.Browser.Cytoscape.Types (CY, Cytoscape, Element, elementJObject)
import Genetics.Browser.Events (Event(..), EventLocation, EventRange, Location, Range, _eventLocation, handleRange)
import Genetics.Browser.Units (_BpMBp, _Chr, _MBp)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX)


-- TODO: elemsUrl should be safer. Maybe it should cache too, idk
type State = { cy :: Maybe Cytoscape
             , elemsUrl :: String
             }

data Query r a
  = Initialize String a
  | Reset a
  | Filter (Predicate Element) a
  | EventFromCy Cytoscape.ParsedEvent (H.SubscribeStatus -> a)
  | RecvEvent (Event r) a

data Output r
  = SendEvent (Event r)

type Effects eff = ( cy :: CY
                   , ajax :: AJAX
                   , console :: CONSOLE
                   , exception :: EXCEPTION
                   , avar :: AVAR | eff)

data Slot = Slot
derive instance eqCySlot :: Eq Slot
derive instance ordCySlot :: Ord Slot


type HandledEvents r = ( range :: Range | r )

type PossibleEvents r = ( location :: Location | r)

component :: ∀ rq rm eff. H.Component HH.HTML (Query (HandledEvents rq)) Unit (Output (PossibleEvents rm)) (Aff (Effects eff))
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
  render :: State -> H.ComponentHTML (Query (HandledEvents rq))
  render = const $ HH.div [ HP.ref (H.RefLabel "cy")
                          , HP.id_ "cyDiv"
                          -- , HP.prop
                          ] []


  -- for some reason having an explicit forall makes the rest of the file not get parsed by purs-ide...
  -- getElements :: ∀ eff'. String -> Aff (ajax :: AJAX | eff') (CyCollection Element)
  getElements :: _
  getElements url = Affjax.get url <#> (\r -> Cytoscape.unsafeParseCollection r.response)

  -- getAndSetElements :: ∀ eff'. String -> Cytoscape -> Aff (ajax :: AJAX, cy :: CY | eff') Unit
  getAndSetElements :: _
  getAndSetElements url cy = do
    eles <- getElements url
    liftEff $ Cytoscape.graphAddCollection cy eles


  eval :: (Query (HandledEvents rq)) ~> H.ComponentDSL State (Query (HandledEvents rq)) (Output (PossibleEvents rm)) (Aff (Effects eff))
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

          H.subscribe $ H.eventSource (Cytoscape.onClick cy) $ Just <<< H.request <<< EventFromCy
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


    EventFromCy (ParsedEvent pev) reply -> do

      case pev.target of
          Left el -> do
            case cyParseEventLocation el of
              Nothing -> liftEff $ log "Error when parsing chr, pos of cytoscape event"
              Just obj' -> H.raise $ SendEvent $ Event $ inj _eventLocation obj'

          Right cy -> pure unit

      pure $ reply H.Listening


    RecvEvent (Event v) next -> do

      H.gets _.cy >>= case _ of
        Nothing -> pure next
        Just cy -> do
          liftEff $ log "received event"
          liftEff $ log $ unsafeStringify v

          (default (pure unit)
            # handleRange \ran -> do

              let pred el = case cyParseEventLocation el of
                    Nothing -> false
                    Just loc -> loc.chr /= ran.chr

              graphColl <- liftEff $ Cytoscape.graphGetCollection cy
              let eles = filter (wrap pred) graphColl
              _ <- liftEff $ Cytoscape.graphRemoveCollection eles
              pure unit

            ) v


          pure next


-- TODO this should be less ad-hoc, somehow. future probs~~~
cyParseEventLocation :: Element -> Maybe Location
cyParseEventLocation el = do
  loc <- elementJObject el ^? ix "data" <<< _Object <<< ix "lrsLoc"
  chr <- loc ^? _Object <<< ix "chr" <<< _String <<< re _Chr
           -- ridiculous.
  pos <- loc ^? _Object <<< ix "pos" <<< _Number
                  <<< re _MBp <<< re _BpMBp
  pure $ { chr, pos }
