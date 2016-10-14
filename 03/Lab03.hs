-- Lab03 Christoph Mende
import Graphics
import XML

graphic :: Graphic
graphic = rectangle 500 1 ++ circle 100

main :: IO ()
main = writeFile "graphic.svg" (xmlToString (toSVG graphic))
