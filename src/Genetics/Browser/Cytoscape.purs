module Genetics.Browser.Cytoscape where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut.Core (JArray)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Genetics.Browser.Cytoscape.Types (CY, CyCollection, CyEvent, Cytoscape, Element)
import Unsafe.Coerce (unsafeCoerce)


-- TODO: move to separate module
newtype Layout = Layout String

circle :: Layout
circle = Layout "circle"

foreign import cytoscapeImpl :: ∀ eff. Nullable HTMLElement
                             -> Nullable JArray
                             -> Eff (cy :: CY | eff) Cytoscape

-- TODO: just realized the cytoscape constructor actually doesn't take a CyCollection,
-- but an array of Json...
-- makes more sense, too, since we need a Cytoscape to create a CyCollection!
cytoscape :: forall eff.
             Maybe HTMLElement
          -> Maybe JArray
          -> Eff (cy :: CY | eff) Cytoscape
cytoscape htmlEl els = liftEff $ cytoscapeImpl (toNullable htmlEl) (toNullable els)


unsafeParseCollection :: Foreign -> CyCollection
unsafeParseCollection = unsafeCoerce

foreign import graphAddCollection :: ∀ eff. Cytoscape
                                 -> CyCollection
                                 -> Eff (cy :: CY | eff) Unit

foreign import graphGetCollection :: ∀ eff. Cytoscape -> Eff (cy :: CY | eff) CyCollection


foreign import runLayout :: forall eff.
                            Cytoscape
                         -> Layout
                         -> Eff (cy :: CY | eff) Unit

-- Used to recalculate the container bounds; if the div moves for some reason (other stuff on the page),
-- mouse clicks are off until this is called.
foreign import resizeContainer :: forall eff. Cytoscape -> Eff (cy :: CY | eff) Unit

foreign import onEventImpl :: ∀ eff a.
                              Cytoscape
                           -> String
                           -> (CyEvent -> Eff (cy :: CY | eff) a)
                           -> Eff (cy :: CY | eff) Unit


-- TODO: move all event stuff to separate module
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


-- is `graphRemoveWithCollection` or similar a better name? ...
foreign import graphRemoveCollection :: ∀ eff.
                                 CyCollection
                              -> Eff (cy :: CY | eff) CyCollection

foreign import graphRemoveAll :: forall eff. Cytoscape -> Eff (cy :: CY | eff) Unit
