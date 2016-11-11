module Menu where

import           ReadInt (readIntRange)

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
