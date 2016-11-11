module ReadInt where

readInt :: String -> Either String Int
readInt str =
  case reads str of
    [(n,"")] -> Right n
    _        -> Left err
  where
    err = "Die Zeichenkette " ++ str ++ " ist keine Zahl"

rangeCheck :: Int -> Int -> Int -> Either String Int
rangeCheck minN maxN n
  | n < minN = Left "Die Zahl ist zu klein"
  | n > maxN = Left "Die Zahl ist zu gross"
  | otherwise = Right n

foldEither :: (a -> b) -> Either b a -> b
foldEither _ (Left x)  = x
foldEither f (Right x) = f x

(>>-) :: Either c a -> (a -> Either c b) -> Either c b
(>>-) (Right x) f = foldEither f (Right x)
(>>-) (Left x) _  = Left x

readIntRange :: Int -> Int -> String -> Either String Int
readIntRange minN maxN n = readInt n >>- rangeCheck minN maxN
