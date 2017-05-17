module Genetics.Browser.Cytoscape where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import DOM.HTML.Types (HTMLElement)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Genetics.Browser.Feature (Feature(..))
import Genetics.Browser.Types (Cytoscape, CY, Biodalliance)
import Genetics.Browser.Units (class HCoordinate, Bp(..), MBp(..), bp)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxResponse, get)
import Unsafe.Coerce (unsafeCoerce)


foreign import cytoscapeImpl :: ∀ eff. HTMLElement
                             -> Nullable (CyCollection CyElement)
                             -> Eff (cy :: CY | eff) Cytoscape

cytoscape :: forall eff.
             HTMLElement
          -> Maybe (CyCollection CyElement)
          -> Aff (cy :: CY | eff) Cytoscape
cytoscape htmlEl els = liftEff $ cytoscapeImpl htmlEl (toNullable els)

foreign import cyAdd :: ∀ eff. Cytoscape
                     -> CyCollection CyElement
                     -> Eff (cy :: CY | eff) (CyCollection CyElement)

foreign import cyFilter :: ∀ a.
                           Fn2 a Int Boolean
                        -> Cytoscape
                        -> CyCollection a

foreign import runLayout :: forall eff.
                            Cytoscape
                         -> Layout
                         -> Eff (cy :: CY | eff) Unit

foreign import resize :: forall eff. Cytoscape -> Eff (cy :: CY | eff) Unit

-- TODO: move to separate module
newtype Layout = Layout String

circle :: Layout
circle = Layout "circle"


foreign import data CyElement :: Type
foreign import data CyCollection :: Type -> Type
foreign import data CyId :: Type
foreign import data CyEvent :: Type

-- TODO: move all event stuff to separate module
newtype ParsedEvent = ParsedEvent { cy :: Cytoscape
                                  , target :: Either CyElement Cytoscape
                                  }

-- pretty nasty. would be better using Foreign, but we need to compare two things of different types...
-- maybe something like
-- _.target >>= parseCy cy <|> parseEl cy
-- where parseCy :: Cytoscape -> Foreign -> F Cytoscape
-- and   parseEl :: Cytoscape -> Foreign -> F CyElement
foreign import parseEventImpl :: forall a b.
                                 (a -> Either a b)
                              -> (b -> Either a b)
                              -> CyEvent
                              -> ParsedEvent


parseEvent :: CyEvent -> ParsedEvent
parseEvent = parseEventImpl Left Right


unsafeParseCollection :: Foreign -> CyCollection CyElement
unsafeParseCollection = unsafeCoerce

-- ajaxAddEles :: forall eff. Cytoscape -> String -> Aff _ (CyCollection CyElement)
ajaxAddEles :: forall eff. Cytoscape -> String -> Aff (cy :: CY, ajax :: AJAX | eff) (CyCollection CyElement)
ajaxAddEles cy url = do
  resp <- get url :: ∀ eff. Affjax (cy :: CY | eff) Foreign
  pure $ unsafeParseCollection resp.response

-- foreign import setOn :: ∀ eff. Cytoscape -> Eff (cy :: CY | eff) Unit
-- TODO: the callback should really be an Eff too, but w/e
foreign import onEventImpl :: ∀ eff a.
                              Cytoscape
                           -> String
                           -> (CyEvent -> Eff (cy :: CY | eff) a)
                           -> Eff (cy :: CY | eff) Unit

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


-- this doesn't work -- has to keep track of removed elements, or reload elements from scratch.
resetFilter :: _
               -- ... applicative for ->?
resetFilter cy = cyAdd cy $ cyFilter (mkFn2 $ \_ _ -> true) cy
-- resetFilter cy = cyAdd cy $ cyFilter (mkFn2 $ const true) cy



foreign import removeElements :: ∀ eff.
                                 CyCollection CyElement
                              -> Eff (cy :: CY | eff) (CyCollection CyElement)

foreign import removeAllElements :: forall eff. Cytoscape -> Eff (cy :: CY | eff) Unit

foreign import filterElements :: ∀ a.
                                 Fn2 a Int Boolean
                              -> CyCollection a
                              -> CyCollection a

filterElements' :: ∀ a. (a -> Int -> Boolean) -> CyCollection a -> CyCollection a
filterElements' p = filterElements (mkFn2 p)

foreign import cyFilterElements :: ∀ eff.
                                   Cytoscape
                                -> (CyElement -> Boolean)
                                -> Eff (cy :: CY | eff) (CyCollection CyElement)
