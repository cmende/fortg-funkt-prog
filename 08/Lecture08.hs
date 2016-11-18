module Lecture08 where


import           Data.Char (toUpper)
import           Prelude   hiding (Monad (..), mapM)


class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  m >> n = m >>= const n


-- empty <> m = m
-- m <> empty = m
-- (m <> n) <> o = m <> (n <> o)

-- return a >>= f = f a
-- m >>= return = m
-- (m >>= f) >>= g   =   m >>= (\x -> f x >>= g)


-- main :: IO ()
-- main =
  -- return "a" >>= putStrLn
  -- putStrLn "a"

  -- putStrLn "a" >>= return
  -- putStrLn "a"

  -- (putStrLn "a" >>= putStrLn) >>= putStrLn
  -- putStrLn "a" >>= (\x -> putStrLn x >>= putStrLn)


instance Monad Maybe where
  -- return :: a -> Maybe a
  return = Just

  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  (Just x) >>= k = k x


data Base = U | A | C | G
  deriving Show
type RNA = [Base]

parseBase :: Char -> Maybe Base
parseBase = parseBaseUpper . toUpper
  where
    parseBaseUpper 'U' = Just U
    parseBaseUpper 'A' = Just A
    parseBaseUpper 'C' = Just C
    parseBaseUpper 'G' = Just G
    parseBaseUpper _   = Nothing

parseRNA :: String -> Maybe RNA
parseRNA [] = return []
parseRNA (c:cs) = do
  b <- parseBase c
  r <- parseRNA cs
  return (b:r)
  -- parseBase c >>= (\ b -> (parseRNA cs >>= \r -> return (b:r)))


-- instance Monad (Either e) where


mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = do
 b <- f x
 bs <- mapM f xs
 return (b:bs)

parseRNA' :: String -> Maybe RNA
parseRNA' = mapM parseBase
