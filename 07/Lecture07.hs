module Lecture07 where


import           Prelude hiding (Functor)

-- map :: (a -> b) -> [a] -> [b]

-- map :: (a -> b) -> List a -> List b

-- mapTree :: (a -> b) -> Tree a -> Tree b

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)


-- mapF :: (a -> b) -> f a -> f b


-- data Bool = True | False

-- data Maybe a = Just a | Nothing

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- instance Functor Int where

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = mapMaybe

-- fmap id = id
-- fmap (f . g) = fmap f . fmap g


-- main :: IO ()

-- public static String sideEffect() {
--  System.out.println("Hallo");
--  return "result";
-- }

-- IO String

main :: IO ()
main =
  getLine >>= \str ->
  putStrLn (str ++ "!") >>
  putStrLn "Text" >>
  putStrLn str

main2 :: IO ()
main2 = do
  str <- getLine
  putStrLn (str ++ "!")
  putStrLn "Text"
  putStrLn str


getLineSuffix :: IO String
getLineSuffix = do
  str <- getLine
  return (str ++ "!")


getLineSuffixF :: IO String
getLineSuffixF = fmap (++ "!") getLine

-- fmap :: (a -> b) -> IO a -> IO B
