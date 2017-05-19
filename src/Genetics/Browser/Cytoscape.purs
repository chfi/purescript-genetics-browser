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
import Genetics.Browser.Types (CY, Cytoscape)
import Unsafe.Coerce (unsafeCoerce)

foreign import data CyElement :: Type
foreign import data CyCollection :: Type -> Type
foreign import data CyEvent :: Type

-- TODO: move to separate module
newtype Layout = Layout String

circle :: Layout
circle = Layout "circle"

foreign import cytoscapeImpl :: ∀ eff. HTMLElement
                             -> Nullable (CyCollection CyElement)
                             -> Eff (cy :: CY | eff) Cytoscape

cytoscape :: forall eff.
             HTMLElement
          -> Maybe (CyCollection CyElement)
          -> Eff (cy :: CY | eff) Cytoscape
cytoscape htmlEl els = liftEff $ cytoscapeImpl htmlEl (toNullable els)


unsafeParseCollection :: Foreign -> CyCollection CyElement
unsafeParseCollection = unsafeCoerce

foreign import coreAddCollection :: ∀ eff. Cytoscape
                                 -> CyCollection CyElement
                                 -> Eff (cy :: CY | eff) Unit

-- what does this filter actually do
foreign import coreFilterImpl :: ∀ eff a.
                                 Cytoscape
                              -> (JObject -> Boolean)
                              -> Eff (cy :: CY | eff) (CyCollection a)

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
                                  , target :: Either CyElement Cytoscape
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
                                 CyCollection CyElement
                              -> Eff (cy :: CY | eff) (CyCollection CyElement)

foreign import coreRemoveAllElements :: forall eff. Cytoscape -> Eff (cy :: CY | eff) Unit


foreign import eleGetAllData :: forall eff.
                                CyElement
                             -> Eff (cy :: CY | eff) JObject

foreign import eleGetDataImpl :: forall eff.
                              CyElement
                           -> String
                           -> Eff (cy :: CY | eff) (Nullable Json)

eleGetData :: forall eff. CyElement -> String -> Eff (cy :: CY | eff) (Maybe Json)
eleGetData el key = toMaybe <$> eleGetDataImpl el key
