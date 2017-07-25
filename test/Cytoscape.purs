module Test.Cytoscape
       ( spec
       ) where

import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..))
import Genetics.Browser.Cytoscape as Cy
import Genetics.Browser.Cytoscape.Collection (contains, emptyCollection, filter, isEdge, isNode)
import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldNotEqual)

foreign import testData :: Array Json

spec :: Spec _ Unit
spec = do
  describe "Cytoscape" do
    it "is not empty if created with elements" $ do
      cy <- liftEff $ Cy.cytoscape Nothing (Just testData)
      eles <- liftEff $ Cy.graphGetCollection cy
      eles `shouldNotEqual` emptyCollection cy

    it "collections can be filtered and recombined" $ do
      cy <- liftEff $ Cy.cytoscape Nothing (Just testData)
      eles <- liftEff $ Cy.graphGetCollection cy

      let edges = filter isEdge eles
          nodes = filter isNode eles
      when (not $ eles `contains` edges) (fail "Graph doesn't contain its edges")
      when (not $ eles `contains` nodes) (fail "Graph doesn't contain its nodes")

      -- union of parts is equal to sum
      (edges <> nodes) `shouldEqual` eles
      eles             `shouldEqual` (edges <> nodes)
      -- union is commutative
      (edges <> nodes) `shouldEqual` (nodes <> edges)
