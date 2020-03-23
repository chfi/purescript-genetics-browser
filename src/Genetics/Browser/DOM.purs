module Genetics.Browser.DOM where


import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.Node as DOM



foreign import unsafeCreateElementImpl
  :: EffectFn2 String String Element


unsafeCreateElement :: { elementType :: String
                       , id :: String }
                    -> Effect Element
unsafeCreateElement args =
  runEffectFn2
    unsafeCreateElementImpl
    args.elementType args.id

foreign import unsafeCreateElementNSImpl
  :: EffectFn3 String String String Element

unsafeCreateSVGElement :: String -> Effect Element
unsafeCreateSVGElement id =
  runEffectFn3
    unsafeCreateElementNSImpl "svg"
                              id
                              "http://www.w3.org/2000/svg"



foreign import appendElem :: Element
                          -> Element
                          -> Effect Unit

-- appendCanvasElem :: Element -> CanvasElement -> Effect Unit
-- appendCanvasElem e c = appendElem e (unsafeCoerce c)
