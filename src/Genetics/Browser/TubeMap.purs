module Genetics.Browser.TubeMap where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, null, toNullable)
import Effect (Effect)
import Foreign (Foreign)
import Genetics.Browser.DOM (unsafeCreateElement, appendElem, unsafeCreateSVGElement)
import Web.DOM (Element)

foreign import data VGNodes :: Type
foreign import data VGTracks :: Type
foreign import data VGReads :: Type

foreign import extractNodes :: Foreign -> VGNodes
foreign import extractTracks :: Foreign -> VGTracks
foreign import extractReads :: VGNodes
                            -> VGTracks
                            -> Foreign
                            -> VGReads

foreign import tubemap :: Foreign

type TubeMapParams r = { svgID :: String
                       , nodes :: VGNodes
                       , tracks :: VGTracks
                       | r
                       }

foreign import createTubeMapImpl :: TubeMapParams (reads :: Nullable VGReads)
                                 -> Effect Unit

createTubeMap :: TubeMapParams (reads :: Maybe VGReads)
              -> Effect Unit
createTubeMap params =
  createTubeMapImpl { svgID: params.svgID
                    , nodes: params.nodes
                    , tracks: params.tracks
                    , reads: toNullable params.reads }


foreign import isInitialized :: Effect Unit

-- Create and append the tubemap DOM elements to the given element
initTubeMapDOM :: Element
               -> Effect Unit
initTubeMapDOM cont = do
  let svgID = "svg"
  div <- unsafeCreateElement { elementType: "div"
                             , id: "tubeMap" }
  svg <- unsafeCreateSVGElement svgID
  -- svg <- unsafeCreateElement { elementType: "svg"
  --                            , id: svgID }

  appendElem div svg
  appendElem cont div




-- foreign import createTubeMap :: VGNodes
--                              -> VGTracks
--                              -> VGReads
--                              -> Effect Unit
