module Genetics.Browser.Cytoscape where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)

foreign import data Cytoscape :: Type
foreign import data BioDalliance :: Type

foreign import data CY :: Effect


-- from cy docs:
-- elements : An array of elements specified as plain objects. For convenience, this option can alternatively be specified as a promise that resolves to the elements JSON.

-- TODO should be an Eff, using DOM at least
foreign import cytoscape :: String -> Cytoscape

foreign import setOn :: Cytoscape -> Eff () Unit

foreign import setBDOn :: BioDalliance -> Cytoscape -> Eff () Unit

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
-}

foreign import data CyElement :: Type
foreign import data CyCollection :: Type -> Type
-- foreign import


-- cytoscape events (there are a bunch, we only need a few for now) see http://js.cytoscape.org/#events
-- data CyEvent = OnClick

-- newtype CyEvent = CyEvent Cytoscape target type namespace timestamp

-- cySetOnClick :: Cytoscape ->


-- there are also collection events, which I think may be more important...
-- http://js.cytoscape.org/#events/collection-events
-- e.g. events for selecting an element from a collection.

data CyThing a  = CyCore Cytoscape
                | CyCollection a
                | CyLayout
                | CyAni
                | CyThing a
