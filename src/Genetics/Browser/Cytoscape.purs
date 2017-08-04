module Genetics.Browser.Cytoscape
       ( cytoscape
       , graphAddCollection
       , graphGetCollection
       , graphRemoveCollection
       , graphRemoveAll
       , unsafeParseCollection
       , runLayout
       , resizeContainer
       , ParsedEvent(..)
       , parseEvent
       , onEvent
       , onClick
       , Layout
       , circle
       ) where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut.Core (JArray)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Genetics.Browser.Cytoscape.Collection (CyCollection)
import Genetics.Browser.Cytoscape.Types (CY, CyEvent, Cytoscape, Element)
import Unsafe.Coerce (unsafeCoerce)


-- TODO: move to separate module
-- | Wrapper for layout names
newtype Layout = Layout String
circle :: Layout
circle = Layout "circle"

foreign import cytoscapeImpl :: ∀ eff. Nullable HTMLElement
                             -> Nullable JArray
                             -> Eff (cy :: CY | eff) Cytoscape

-- | Creates a Cytoscape.js graph instance.
-- | `htmlEl` is the element to place it in; if Nothing, the graph is headless.
-- | `els` is the array of elements to fill the browser with.
cytoscape :: forall eff.
             Maybe HTMLElement
          -> Maybe JArray
          -> Eff (cy :: CY | eff) Cytoscape
cytoscape htmlEl els = liftEff $ cytoscapeImpl (toNullable htmlEl) (toNullable els)


unsafeParseCollection :: Foreign -> CyCollection Element
unsafeParseCollection = unsafeCoerce

-- | Add a Collection of elements to the graph
foreign import graphAddCollection :: ∀ eff.
                                     Cytoscape
                                  -> CyCollection Element
                                  -> Eff (cy :: CY | eff) Unit

-- | Get all elements in the graph
foreign import graphGetCollection :: ∀ eff.
                                     Cytoscape
                                  -> Eff (cy :: CY | eff) (CyCollection Element)

-- | Apply a layout to the graph
foreign import runLayout :: forall eff.
                            Cytoscape
                         -> Layout
                         -> Eff (cy :: CY | eff) Unit

-- | Recalculate the container bounds, fixes the mouse click offset
foreign import resizeContainer :: forall eff.
                                  Cytoscape
                               -> Eff (cy :: CY | eff) Unit

foreign import onEventImpl :: ∀ eff a.
                              Cytoscape
                           -> String
                           -> (CyEvent -> Eff (cy :: CY | eff) a)
                           -> Eff (cy :: CY | eff) Unit


-- TODO: This is poorly named and clumsy
-- | Basic wrapper over the Cy.js on-click events
newtype ParsedEvent = ParsedEvent { cy :: Cytoscape
                                  , target :: Either Element Cytoscape
                                  }

foreign import parseEventImpl :: forall a b.
                                 (a -> Either a b)
                              -> (b -> Either a b)
                              -> CyEvent
                              -> ParsedEvent

parseEvent :: CyEvent -> ParsedEvent
parseEvent = parseEventImpl Left Right


-- | Set a Cy.js event handler
-- | `ev` is the string identifier of the event type
onEvent :: forall a eff.
           Cytoscape
        -> String
        -> (ParsedEvent -> Eff (cy :: CY | eff) a)
        -> Eff (cy :: CY | eff) Unit
onEvent cy ev f = onEventImpl cy ev (f <<< parseEvent)


onClick :: ∀ eff.
           Cytoscape
        -> (ParsedEvent -> Eff (cy :: CY | eff) Unit)
        -> Eff (cy :: CY | eff) Unit
onClick cy = onEvent cy "click"


-- | Remove a collection of elements from the graph,
-- | Returning the removed elements
foreign import graphRemoveCollection :: ∀ eff.
                                        CyCollection Element
                                     -> Eff (cy :: CY | eff) (CyCollection Element)

-- | Empty the graph
foreign import graphRemoveAll :: forall eff.
                                 Cytoscape
                              -> Eff (cy :: CY | eff) Unit
