import           Graphics

graphic :: Graphic
graphic = rectangle 500 1 <> circle 100

graphic2 :: Graphic
graphic2 = colored Red graphic

main :: IO ()
main = writeFile "graphic.svg" (toSVG graphic2)
