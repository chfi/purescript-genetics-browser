module Graphics.SVG
       ( SVG
       , SVGContext
       , SVGTransform
       , SVGElement
       , renderSVG
       , initialSVG
       , setStrokeStyle
       , setFillStyle
       , circle
       , line
       , path
       , rect
       , translate
       , scale
       ) where

-- TODO: This deserves to be in a separate package (and better modularized)
import Control.Monad.Eff (Eff)
import Control.Monad.State (get, put)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Writer (Writer, tell)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document as Document
import DOM.Node.Element as Element
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Element, elementToNode)
import Data.Array (uncons)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Traversable (intercalate, traverse_)
import Data.Tuple (Tuple(..))
import Prelude


createElementSVG :: ∀ eff. String -> Eff (dom :: DOM | eff) Element
createElementSVG tag = do
  win <- window
  doc <- document win
  Document.createElementNS
    (Just "http://www.w3.org/2000/svg")
    tag
    (htmlDocumentToDocument doc)


type Attribute = Tuple String String

setAttributes :: ∀ f eff. Foldable f => f Attribute -> Element -> Eff (dom :: DOM | eff) Unit
setAttributes attrs ele = traverse_ (\ (Tuple n v) -> Element.setAttribute n v ele) attrs

-- | An SVG element is a name with attributes
data SVGElement = SVGElement String (Array Attribute)

data SVGPathOp = SVGPathMoveTo Number Number
               | SVGPathLineTo Number Number
               | SVGPathClose

derive instance eqSVGPathOp :: Eq SVGPathOp

instance showSVGPathOp :: Show SVGPathOp where
  show (SVGPathMoveTo x y) = "M " <> show x <> " " <> show y
  show (SVGPathLineTo x y) = "L " <> show x <> " " <> show y
  show SVGPathClose = "Z"


renderSVGElement :: ∀ eff.
                    SVGElement
                 -> Eff (dom :: DOM | eff) Element
renderSVGElement (SVGElement tag as) = do
  ele <- createElementSVG tag
  setAttributes as ele
  pure ele


-- | Renders some SVGElements, returning an HTML element
renderSVG :: ∀ f eff.
             Foldable f
          => f SVGElement
          -> Eff (dom :: DOM | eff) Element
renderSVG as = do
  win <- window
  doc <- document win
  root <- createElementSVG "g"
  let parent = elementToNode root
  traverse_ (\ svgEle -> do
                rendered <- renderSVGElement svgEle
                appendChild (elementToNode rendered) parent) as
  pure root


derive instance genericSVGElement :: Generic SVGElement _
derive instance eqSVGElement :: Eq SVGElement
instance showSVGElement :: Show SVGElement where
  show x = genericShow x


type SVGTransform = { translate :: { x :: Number, y :: Number }
                    , scale :: { x :: Number, y :: Number}
                    }

showTransform :: SVGTransform -> String
showTransform t = "translate(" <> show t.translate.x <> "," <> show t.translate.y <> ") " <>
                  "scale(" <> show t.scale.x <> "," <> show t.scale.y <> ")"

-- | The SVG context (incomplete)
type SVGContext = { stroke :: String
                  , fill :: String
                  , strokeWidth :: Number
                  , transform :: SVGTransform
                  }

-- | Contains the current state of the SVG context as well as the SVG document as an array of elements
type SVG a = StateT SVGContext (Writer (Array SVGElement)) a

svgToAttribs :: SVGContext -> Array Attribute
svgToAttribs svg = [ Tuple "fill" svg.fill
                   , Tuple "stroke" svg.stroke
                   , Tuple "strokeWidth" (show svg.strokeWidth <> "px")
                   , Tuple "transform" (showTransform svg.transform)
                   ]


-- | The empty/initial SVGContext
initialSVG :: SVGContext
initialSVG = { stroke: "none"
             , fill: "none"
             , strokeWidth: 1.0
             , transform: { translate: { x: 0.0, y: 0.0 }
                          , scale: { x: 1.0, y: 1.0 }
                          }
             }


setStrokeStyle :: String
               -> SVG Unit
setStrokeStyle color = do
  cur <- get
  let cur' = cur { stroke = color }
  put cur'

setFillStyle :: String
             -> SVG Unit
setFillStyle color = do
  cur <- get
  let cur' = cur { fill = color }
  put cur'


-- TODO: it feels like these functions can clean up nicely,
-- using some suitable/more restrictive abstraction, maybe.
-- there's an obvious pattern to many of them:
-- cur <- get; ~stuff~; tell [Element tag attribs]
circle :: Number
       -> Number
       -> Number
       -> SVG Unit
circle x y r = do
  cur <- get
  let circle' = [ Tuple "cx" (show x)
                , Tuple "cy" (show y)
                , Tuple "r" (show r)
                ] <> svgToAttribs cur
  tell [SVGElement "circle" circle']

line :: Number
     -> Number
     -> Number
     -> Number
     -> SVG Unit
line x1 y1 x2 y2 = path [ {x:x1, y:y1}, {x:x2, y:y2} ]


path :: Array { x :: Number, y :: Number }
     -> SVG Unit
path ps = do
  case uncons ps of
    Nothing -> pure unit
    Just { head, tail } -> do
      cur <- get
      let fst = [ SVGPathMoveTo head.x head.y
                ]
          rest = (\p -> SVGPathLineTo p.x p.y) <$> ps
          path' = intercalate " " $ show <$> fst <> rest <> [SVGPathClose]
          attribs = [ Tuple "d" path' ] <> svgToAttribs cur
      tell [SVGElement "path" attribs]


rect :: Number
     -> Number
     -> Number
     -> Number
     -> SVG Unit
rect x1 y1 x2 y2 = do
  cur <- get
  let rect' = [ Tuple "x" (show x1)
              , Tuple "y" (show y1)
              , Tuple "width" (show (x2 - x1))
              , Tuple "height" (show (y2 - y1))
              ] <> svgToAttribs cur
  tell [SVGElement "rect" rect']


translate :: Number
          -> Number
          -> SVG Unit
translate x y = do
  cur <- get
  let t = cur.transform.translate
      t' = t { x = t.x + x, y = t.y + y }
      new = cur { transform = ( cur.transform { translate = t })}
  put new


scale :: Number
      -> Number
      -> SVG Unit
scale x y = do
  cur <- get
  let s = cur.transform.scale
      s' = s { x = s.x * x, y = s.y * y }
      new = cur { transform = ( cur.transform { scale = s })}
  put new
