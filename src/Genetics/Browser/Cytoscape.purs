module Genetics.Browser.Cytoscape where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Genetics.Browser.Feature (Feature(..))
import Genetics.Browser.Types (Cytoscape, CY, Biodalliance)
import Genetics.Browser.Units (class HCoordinate, Bp(..), MBp(..), bp)
import Network.HTTP.Affjax (AJAX, Affjax, get)
import Unsafe.Coerce (unsafeCoerce)




-- from cy docs:
-- elements : An array of elements specified as plain objects. For convenience, this option can alternatively be specified as a promise that resolves to the elements JSON.

-- TODO should be an Eff, using DOM at least!!
foreign import cytoscape :: ∀ eff. String
                         -> Nullable (CyCollection CyElement)
                         -> Eff (cy :: CY | eff) Cytoscape
foreign import cyAdd :: ∀ eff. Cytoscape
                     -> CyCollection CyElement
                     -> Eff (cy :: CY | eff) (CyCollection CyElement)
foreign import cyFilter :: ∀ a.
                           Fn2 a Int Boolean
                        -> Cytoscape
                        -> CyCollection a

-- ajaxCytoscape :: String -> String -> Eff (ajax :: AJAX) Cytoscape
ajaxCytoscape :: _
ajaxCytoscape div url = launchAff $ do
  resp <- get url :: Affjax _ Foreign
  pure $ cytoscape div $ unsafeCoerce resp.response

  -- Cytoscape -> String ->
ajaxAddEles :: _
ajaxAddEles cy url = launchAff $ do
  resp <- get url :: ∀ eff. Affjax (cy :: CY | eff) Foreign
  liftEff $ cyAdd cy $ unsafeCoerce resp.response

-- foreign import setOn :: ∀ eff. Cytoscape -> Eff (cy :: CY | eff) Unit
-- TODO: the callback should really be an Eff too, but w/e
foreign import setOn :: ∀ cbEff eff.
                        Cytoscape
                     -> String
                     -> (CyEvent -> Eff cbEff Unit)
                     -> Eff (cy :: CY | eff) Unit

onClick :: ∀ cbEff eff.
           Cytoscape
        -> (CyEvent -> Eff cbEff Unit)
        -> Eff (cy :: CY | eff) Unit
onClick cy = setOn cy "click"


-- this doesn't work -- has to keep track of removed elements, or reload elements from scratch.
resetFilter :: _
               -- ... applicative for ->?
resetFilter cy = cyAdd cy $ cyFilter (mkFn2 $ \_ _ -> true) cy
-- resetFilter cy = cyAdd cy $ cyFilter (mkFn2 $ const true) cy



-- foreign import setBDOn :: ∀ eff. Biodalliance -> Cytoscape -> Eff (cy :: CY | eff) Unit

-- need to be able to specify stylesheet, elements. other config?
{-
cy.add( eleObj )
Add a specified element to the graph.
eleObj A plain object that specifies the element.
cy.add( eleObjs )
Add the specified elements to the graph.
eleObjs An array of elements specified by plain objects.
cy.add( eles )
Add the specified elements to the graph.
eles A collection of elements.

-- these all return the added elements...
-- ... meaning `cy` is modified.

-- `Collections` are immutable.
-}
-- TODO: we really want this to connect with BD, so Features should be here too.
-- would simplify the BD<->Cy interface, at least.
foreign import data CyElement :: Type
foreign import data CyCollection :: Type -> Type
foreign import data CyId :: Type
foreign import data CyEvent :: Type
-- foreign import


-- cytoscape events (there are a bunch, we only need a few for now) see http://js.cytoscape.org/#events
-- data CyEvent = OnClick

-- newtype CyEvent = CyEvent Cytoscape target type namespace timestamp

-- cySetOnClick :: Cytoscape ->


-- there are also collection events, which I think may be more important...
-- http://js.cytoscape.org/#events/collection-events
-- e.g. events for selecting an element from a collection.

-- effectfully removing elements from a collection...
-- removing doesn't delete the elements, it only removes them from the visible graph. hence Eff

foreign import removeElements :: ∀ eff.
                                 CyCollection CyElement
                              -> Eff (cy :: CY | eff) (CyCollection CyElement)

foreign import filterElements :: ∀ a.
                                 Fn2 a Int Boolean
                              -> CyCollection a
                              -> CyCollection a

filterElements' :: ∀ a. (a -> Int -> Boolean) -> CyCollection a -> CyCollection a
filterElements' p = filterElements (mkFn2 p)

foreign import elesOn :: ∀ eff. String
                      -> (CyEvent -> Unit)
                      -> CyCollection CyElement
                      -> Eff (cy :: CY | eff) Unit
-- foreign import isRemoved :: ∀ eff. CyElement -> Eff (cy :: CY | eff) Boolean
-- isInside :: ∀ eff. CyElement -> Eff (cy :: CY | eff) Boolean
-- isInside el = not <$> isRemoved el
-- foreign import restoreElements :: ∀ a eff. CyCollection a -> Eff (cy :: CY | eff) Unit
-- foreign import cloneElements :: ∀ a eff. CyCollection a ->

-- TODO: there's more to move() than this.

-- foreign import moveElements :: ∀ a. CyCollection a -> CyId -> CyCollection a
-- set function as callback
-- foreign import elesOn :: ∀ a b eff. CyCollection a -> (CyEvent -> b) -> Eff (cy :: CY | eff) Unit
-- foreign import eleId :: CyElement -> CyId

-- data CyThing a  = CyCore Cytoscape
--                 | CyCollection a
--                 | CyLayout
--                 | CyAni
--                 | CyThing a

foreign import cyFilterElements :: ∀ eff.
                                   Cytoscape
                                -> (CyElement -> Boolean)
                                -> Eff (cy :: CY | eff) (CyCollection CyElement)



-- Filters by chromosome -- by appending "Chr" to the current chromosome...
-- TODO: terrible!
foreign import cyFilterByString :: ∀ eff.
                                   Cytoscape
                                -> String
                                -> Eff (cy :: CY | eff) Unit


-- TODO: this is shitty and unsafe
-- it doesn't even take the coordinate unit into account!
-- CyElement should probably have the extra data in a type parameter
type P = { chr :: String, pos :: MBp }
foreign import elDataImpl :: (P -> Maybe P) -> (Maybe P)
                          -> CyElement
                          -> Maybe P


elData :: CyElement -> Maybe P
elData = elDataImpl Just Nothing

-- this type is false: sometimes the evt target is the core, or a collection. maybe.
foreign import evtTarget :: CyEvent -> CyElement
