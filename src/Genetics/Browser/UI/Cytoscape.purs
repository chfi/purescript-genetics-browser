module Genetics.Browser.UI.Cytoscape
       where

import Prelude
import Genetics.Browser.Biodalliance as Biodalliance
import Genetics.Browser.Cytoscape as Cytoscape
import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot as QTL
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Const (Const(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (type (<\/>), Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Nullable (toNullable)
import Genetics.Browser.Cytoscape (ajaxAddEles, cytoscape)
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Source.QTL (fetch)
import Genetics.Browser.Types (BD, Biodalliance, CY, Cytoscape, Renderer)
import Genetics.Browser.Units (Bp(..))
import Global.Unsafe (unsafeStringify)
import Halogen.VDom.Driver (runUI)


type State = { cy :: Maybe Cytoscape }

data Query a
  = Initialize String a

type CyEffects eff = (cy :: CY | eff)


component :: ∀ eff. H.Component HH.HTML Query Unit Void (Aff _)
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { cy: Nothing }

  -- doesn't actually render anything...
  render :: State -> H.ComponentHTML Query
  render = const $ HH.div [ HP.ref (H.RefLabel "cy") ] []

  eval :: Query ~> H.ComponentDSL State Query Void (Aff _)
  -- eval :: AceQuery ~> H.ComponentDSL AceState AceQuery AceOutput (Aff (AceEffects eff))
  eval = case _ of
    Initialize div next -> do
      let cy = cytoscape div (toNullable Nothing)
      _ <- liftEff $ ajaxAddEles cy "http://localhost:8080/eles.json"
      _ <- liftEff $ log "what the hek"
      H.modify (_ { cy = Just cy })
      pure next
      -- pure next

data Slot = Slot
derive instance eqCySlot :: Eq Slot
derive instance ordCySlot :: Ord Slot
