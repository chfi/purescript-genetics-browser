module Genetics.Browser.Cytoscape.Collection where

import Data.Argonaut ((.?))
import Data.Argonaut.Core (JObject, JArray)
import Data.Either (Either(..))
import Data.Foldable (all)
import Genetics.Browser.Cytoscape.Types (Cytoscape, Element, elementJson)
import Genetics.Browser.Feature.Foreign (parsePath)
import Prelude

foreign import data CyCollection :: Type

foreign import emptyCollection :: Cytoscape -> CyCollection
foreign import collectionJson :: CyCollection -> JArray

foreign import union :: CyCollection
                     -> CyCollection
                     -> CyCollection

foreign import connectedEdges :: CyCollection
                              -> CyCollection

foreign import connectedNodes :: CyCollection
                              -> CyCollection

foreign import filter :: (Element -> Boolean)
                      -> CyCollection
                      -> CyCollection

foreign import isNode :: Element -> Boolean
foreign import isEdge :: Element -> Boolean


evenEdges :: CyCollection -> CyCollection
evenEdges =
  let evenId el = case (elementJson el) .? "id" of
        Left _  -> false
        Right i -> i `mod` 2 == 0
      -- get all nodes with even IDs
  in filter ((&&) <$> isNode <*> evenId)
      -- get the connected edges (discarding the nodes)
     >>> connectedEdges


evenEdgesWithNodes :: CyCollection -> CyCollection
evenEdgesWithNodes coll =
  let evenId el = case (elementJson el) .? "id" of
        Left _  -> false
        Right i -> i `mod` 2 == 0
      edges = filter (all id [isNode, evenId]) coll
  in coll `union` edges

-- same as above (i think)
evenEdgesWithNodes' :: CyCollection -> CyCollection
evenEdgesWithNodes' =
  let evenId el = case (elementJson el) .? "id" of
        Left _  -> false
        Right i -> i `mod` 2 == 0
  in union <$> filter (all id [isNode, evenId]) <*> connectedNodes


locPred :: String -> JObject -> Boolean
locPred chr obj = case obj `parsePath` ["lrsLoc"] of
  Left _  -> false
  Right l -> case l .? "chr" of
    Left _ -> false
    Right c -> chr == c

edgesLoc :: String -> CyCollection -> CyCollection
edgesLoc chr = filter (locPred chr <<< elementJson)
