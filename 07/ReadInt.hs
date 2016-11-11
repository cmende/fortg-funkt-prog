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


-- Interactive Menu
menu :: [String] -> IO Int
menu xs = do
  -- foldr (\ x y -> y >> putStrLn x) (return ()) xs
  putStrLn (unlines (zipWith showLine [1..] xs))
  n <- getLine
  case readIntRange 1 (length xs) n of
    Left _  -> menu xs
    Right x -> return x
  where
    showLine :: Int -> String -> String
    showLine x y = show x ++ ". " ++ y

menuSelect :: [(String,IO a)] -> IO a
menuSelect xs = do
  n <- menu (map fst xs)
  snd (xs !! (n - 1))


-- Word Count
data Action = Chars | Words | Lines
  deriving (Enum,Show)

printAction :: Action -> String -> Int
printAction Chars = length
printAction Words = length . words
printAction Lines = length. lines

wordCount :: FilePath -> IO ()
wordCount p = do
  c <- readFile p
  menuSelect (listWordCount c)
  where
    actions = [Chars .. Lines]
    listWordCount c =
      map (\ x -> ("Count "++show x++" in file",
        print (printAction x c))) actions


list1 :: [String]
list1 = ["foo","baz","bar"]

list2 :: [(String,IO ())]
list2 = [("Hello World",putStrLn "Hello World")]
