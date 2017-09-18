module Genetics.Browser.Cytoscape
       ( cytoscape
       , graphAddCollection
       , graphGetCollection
       , graphRemoveCollection
       , graphRemoveAll
       , unsafeParseCollection
       , runLayout
       , resizeContainer
       -- , ParsedEvent(..)
       -- , parseEvent
       , onEvent
       , onClick
       , Layout
       , circle
       ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, runEffFn1, runEffFn2, runEffFn3)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut (Json)
import Data.Argonaut.Core (JArray)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3, runFn3)
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

foreign import cytoscapeImpl :: ∀ eff.
                                EffFn2 (cy :: CY | eff)
                                (Nullable HTMLElement)
                                (Nullable JArray)
                                Cytoscape

-- | Creates a Cytoscape.js graph instance.
-- | `htmlEl` is the element to place it in; if Nothing, the graph is headless.
-- | `els` is the array of elements to fill the browser with.
cytoscape :: forall eff.
             Maybe HTMLElement
          -> Maybe JArray
          -> Eff (cy :: CY | eff) Cytoscape
cytoscape htmlEl els = runEffFn2 cytoscapeImpl (toNullable htmlEl) (toNullable els)


unsafeParseCollection :: Foreign -> CyCollection Element
unsafeParseCollection = unsafeCoerce

-- | Add a Collection of elements to the graph
foreign import graphAddCollectionImpl :: ∀ eff.
                                         EffFn2 (cy :: CY | eff)
                                         Cytoscape
                                         (CyCollection Element)
                                         Unit

graphAddCollection :: ∀ eff.
                      Cytoscape
                   -> CyCollection Element
                   -> Eff (cy :: CY | eff) Unit
graphAddCollection = runEffFn2 graphAddCollectionImpl

-- | Get all elements in the graph
foreign import graphGetCollectionImpl :: ∀ eff.
                                         EffFn1 (cy :: CY | eff)
                                         Cytoscape
                                         (CyCollection Element)

graphGetCollection :: ∀eff.
                      Cytoscape
                   -> Eff (cy :: CY | eff) (CyCollection Element)
graphGetCollection = runEffFn1 graphGetCollectionImpl

-- | Apply a layout to the graph
foreign import runLayoutImpl :: forall eff.
                                EffFn2 (cy :: CY | eff)
                                Cytoscape
                                Layout
                                Unit

runLayout :: forall eff.
             Cytoscape
          -> Layout
          -> Eff (cy :: CY | eff) Unit
runLayout = runEffFn2 runLayoutImpl

-- | Recalculate the container bounds, fixes the mouse click offset
foreign import resizeContainerImpl :: forall eff.
                                      EffFn1 (cy :: CY | eff)
                                      Cytoscape
                                      Unit

resizeContainer :: forall eff.
                   Cytoscape
                -> Eff ( cy :: CY | eff ) Unit
resizeContainer = runEffFn1 resizeContainerImpl

foreign import onEventImpl :: ∀ eff a.
                              EffFn3 (cy :: CY | eff)
                              Cytoscape
                              String
                              (CyEvent -> Eff (cy :: CY | eff) a)
                              Unit


-- TODO: This is poorly named and clumsy
-- | Basic wrapper over the Cy.js on-click events
-- newtype ParsedEvent = ParsedEvent { cy :: Cytoscape
--                                   , target :: Either Element Cytoscape
--                                   }

-- foreign import parseEventImpl :: forall a b.
--                                  Fn3
--                                  (a -> Either a b)
--                                  (b -> Either a b)
--                                  CyEvent
--                                  ParsedEvent

-- parseEvent :: CyEvent -> ParsedEvent
-- parseEvent = runFn3 parseEventImpl Left Right


-- | Set a Cy.js event handler
-- | `ev` is the string identifier of the event type
onEvent :: forall a eff.
           Cytoscape
        -> String
        -> (CyEvent -> Eff (cy :: CY | eff) a)
        -> Eff (cy :: CY | eff) Unit
onEvent cy ev f = runEffFn3 onEventImpl cy ev f


onClick :: ∀ eff.
           Cytoscape
        -> (CyEvent -> Eff (cy :: CY | eff) Unit)
        -> Eff (cy :: CY | eff) Unit
onClick cy = onEvent cy "click"


-- | Remove a collection of elements from the graph,
-- | Returning the removed elements
foreign import graphRemoveCollectionImpl :: ∀ eff.
                                            EffFn1 (cy :: CY | eff)
                                            (CyCollection Element)
                                            (CyCollection Element)

graphRemoveCollection :: forall eff.
                         CyCollection Element
                      -> Eff ( cy :: CY | eff) (CyCollection Element)
graphRemoveCollection = runEffFn1 graphRemoveCollectionImpl

-- | Empty the graph
foreign import graphRemoveAllImpl :: forall eff.
                                     EffFn1 (cy :: CY | eff)
                                     Cytoscape
                                     Unit

graphRemoveAll :: forall eff.
                  Cytoscape
               -> Eff (cy :: CY | eff) Unit
graphRemoveAll = runEffFn1 graphRemoveAllImpl
