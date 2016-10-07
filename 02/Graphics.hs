-- Lab02 Christoph Mende
module Graphics where

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

data Graphic = Empty
             | Item Object Graphic
             deriving Show

single :: Object -> Graphic
single o = Item o Empty

(<>) :: Graphic -> Graphic -> Graphic
(<>) Empty g          = g
(<>) (Item o Empty) g = Item o g
(<>) (Item _ g1) g2   = g1 <> g2

objToSVG :: Object -> String
objToSVG (Rect (Point ax ay) (Point bx by) s) =
  "<rect x='" ++ show ax ++ "' y='" ++ show ay ++ "' width='" ++ show (bx - ax)
  ++ "' height='" ++ show (by - ay) ++ "' style='" ++ styleToAttr s ++ "' />"
objToSVG (Circle (Point x y) r s) = "<circle x='" ++ show x ++ "' y='" ++ show y
  ++ "' r='" ++ show r ++ "' style='" ++ styleToAttr s ++ "' />"

toSVG :: Graphic -> String
toSVG Empty          = "<svg xmlns='http://www.w3.org/2000/svg'></svg>"
toSVG (Item o Empty) = objToSVG o
toSVG (Item o g)     = "<svg xmlns='http://www.w3.org/2000/svg'>" ++ objToSVG o
  ++ toSVG g ++ "</svg>"

rectangle :: Double -> Double -> Graphic
rectangle x y = Item (Rect (Point 0 0) (Point x y) defaultStyle) Empty

circle :: Double -> Graphic
circle r = Item (Circle (Point 0 0) r defaultStyle) Empty

colored :: Color -> Graphic -> Graphic
colored _ Empty                       = Empty
colored c (Item (Rect p1 p2 _) Empty) = Item (Rect p1 p2 (Style c)) Empty
colored c (Item (Circle p r _) Empty) = Item (Circle p r (Style c)) Empty
colored c (Item o g)                  = colored c (Item o Empty) <> colored c g
