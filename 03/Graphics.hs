-- Lab03 Christoph Mende
module Graphics where

import           XML

data Point = Point Double Double
           deriving Show
data Object = Rect Point Point Style
            | Circle Point Double Style
            deriving Show

data Color = Black | Red | Green | Blue
           deriving Show
data Style = Style Color
           deriving Show

styleToAttr :: Style -> String
styleToAttr (Style Black) = "stroke: black; fill: black"
styleToAttr (Style Red)   = "stroke: red; fill: red"
styleToAttr (Style Green) = "stroke: green; fill: green"
styleToAttr (Style Blue)  = "stroke: blue; fill: blue"

defaultStyle :: Style
defaultStyle = Style Black

type Graphic = [Object]

single :: Object -> Graphic
single o = [o]

objToSVG :: Object -> XML
objToSVG (Rect (Point ax ay) (Point bx by) s) =
  XNode "rect" ["x" := show ax, "y" := show ay, "width" := show (bx - ax),
    "height" := show (by - ay), "style" := styleToAttr s] []
objToSVG (Circle (Point x y) r s) =
  XNode "circle" ["cx" := show x, "cy" := show y, "r" := show r,
    "style" := styleToAttr s] []

toSVG :: Graphic -> XML
toSVG [] = XText ""
toSVG xs =
  XNode "svg" ["xmlns" := "http://www.w3.org/2000/svg"] (map objToSVG xs)

rectangle :: Double -> Double -> Graphic
rectangle x y = [Rect (Point 0 0) (Point x y) defaultStyle]

circle :: Double -> Graphic
circle r = [Circle (Point r r) r defaultStyle]

colored :: Color -> Graphic -> Graphic
colored _ []             = []
colored c [Rect p1 p2 _] = [Rect p1 p2 (Style c)]
colored c [Circle p r _] = [Circle p r (Style c)]
colored c (x:xs)         = colored c [x] ++ colored c xs
