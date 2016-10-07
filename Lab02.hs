import Graphics

graphic :: Graphic
graphic = rectangle 500 1 <> circle 100

main :: IO ()
main = writeFile "graphic.svg" (toSVG graphic)
