module StateMonad where

import           Control.Monad (ap)


data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving Show

numberTree' :: Tree a -> Int -> (Tree Int,Int)
numberTree' Empty x = (Empty,x)
numberTree' (Node l _ r) x =
  let
    (tl,xl) = numberTree' l (x + 1)
    (tr,xr) = numberTree' r xl
  in (Node tl x tr,xr)

tree1 :: Tree String
tree1 = Node (Node Empty "a" (Node Empty "b" Empty)) "c" (Node (Node Empty "d" Empty) "e" Empty)


data State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State (\ s -> let (a,s2) = g s in (f a,s2))

instance Monad (State s) where
  -- return :: a -> State s a
  return x = State (\ s -> (x,s))

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  (State f) >>= k = State (\ s -> let (a,s2) = f s in runState (k a) s2)

instance Applicative (State s) where
 pure = return
 (<*>) = ap

evalState :: State s a -> s -> a
evalState (State f) s = fst (f s)

get :: State s s
get = State (\ s -> (s,s))

put :: s -> State s ()
-- put x = State (\ _ -> ((),x))
put x = State (const ((),x))

modify :: (s -> s) -> State s ()
modify f = State (\ s -> ((),f s))

-- numberTreeS' :: Tree a -> State Int (Tree Int)

-- numberTreeS :: Tree a -> Tree Int
