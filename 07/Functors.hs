module Functors where

import           Prelude hiding (Functor (..))

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

data Tree a =
    Empty
  | Node (Tree a) a (Tree a)

instance Functor Tree where
  fmap _ Empty          = Empty
  fmap f (Node t1 x t2) = Node (fmap f t1) (f x) (fmap f t2)

instance Functor (Either e) where
  fmap _ (Left x)  = Left x
  fmap f (Right x) = Right (f x)
