module Genetics.Browser.Cytoscape where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import DOM.HTML.Types (HTMLElement)
import Data.Argonaut (Json)
import Data.Argonaut.Core (JObject)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.StrMap (StrMap)
import Genetics.Browser.Cytoscape.Types (CY, CyCollection, CyEvent, Cytoscape, Element)
import Unsafe.Coerce (unsafeCoerce)


-- TODO: move to separate module
newtype Layout = Layout String

circle :: Layout
circle = Layout "circle"

foreign import cytoscapeImpl :: ∀ eff. HTMLElement
                             -> Nullable (CyCollection)
                             -> Eff (cy :: CY | eff) Cytoscape

cytoscape :: forall eff.
             HTMLElement
          -> Maybe (CyCollection)
          -> Eff (cy :: CY | eff) Cytoscape
cytoscape htmlEl els = liftEff $ cytoscapeImpl htmlEl (toNullable els)


unsafeParseCollection :: Foreign -> CyCollection
unsafeParseCollection = unsafeCoerce

foreign import coreAddCollection :: ∀ eff. Cytoscape
                                 -> CyCollection
                                 -> Eff (cy :: CY | eff) Unit


foreign import runLayout :: forall eff.
                            Cytoscape
                         -> Layout
                         -> Eff (cy :: CY | eff) Unit

foreign import resize :: forall eff. Cytoscape -> Eff (cy :: CY | eff) Unit


-- foreign import setOn :: ∀ eff. Cytoscape -> Eff (cy :: CY | eff) Unit
-- TODO: the callback should really be an Eff too, but w/e
foreign import onEventImpl :: ∀ eff a.
                              Cytoscape
                           -> String
                           -> (CyEvent -> Eff (cy :: CY | eff) a)
                           -> Eff (cy :: CY | eff) Unit


-- pretty nasty. would be better using Foreign, but we need to compare two things of different types...
-- maybe something like
-- _.target >>= parseCy cy <|> parseEl cy
-- where parseCy :: Cytoscape -> Foreign -> F Cytoscape
-- and   parseEl :: Cytoscape -> Foreign -> F CyElement

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



foreign import collRemoveElements :: ∀ eff.
                                 CyCollection
                              -> Eff (cy :: CY | eff) CyCollection

foreign import coreRemoveAllElements :: forall eff. Cytoscape -> Eff (cy :: CY | eff) Unit


foreign import eleGetAllData :: forall eff.
                                Element
                             -> Eff (cy :: CY | eff) JObject

foreign import eleGetDataImpl :: forall eff.
                              Element
                           -> String
                           -> Eff (cy :: CY | eff) (Nullable Json)

eleGetData :: forall eff. Element -> String -> Eff (cy :: CY | eff) (Maybe Json)
eleGetData el key = toMaybe <$> eleGetDataImpl el key
