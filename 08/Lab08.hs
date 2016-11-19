module Lab08 where

--import           Control.Monad (ap)
import           Prelude hiding (Monad (..))

class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  m >> n = m >>= const n


data Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  -- return :: a -> Identity a
  return = Identity

  -- (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  Identity x >>= k = k x

instance Monad (Either e) where
  -- return :: a -> Either e a
  return = Right

  -- (>>=) :: Either e a -> (a -> Either e b) -> Either e b
  Left x >>= _ = Left x
  Right x >>= k = k x

instance Monoid m => Monad ((,) m) where
  -- return :: a -> (m,a)
  return x = (mempty,x)

  -- (>>=) :: (m,a) -> (a -> (m,b)) -> (m,b)
  (_,y) >>= k = k y

instance Monad ((->) r) where
  -- return :: a -> (r -> a)
  return = const

  -- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
  x >>= k = \ r -> k (x r) r


-- List monad

-- return a >>= f = f a
-- m >>= return = m
-- (m >>= f) >>= g   =   m >>= (\x -> f x >>= g)

instance Monad [] where
  -- return :: a -> [a]
  return x = [x]

  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  [] >>= _     = []
  (x:xs) >>= k = k x ++ xs >>= k

cross :: [a] -> [b] -> [(a,b)]
cross xs ys = xs >>= (\ x -> ys >>= (\ y -> [(x,y)]))


-- State monad
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

--instance Applicative (State s) where
--  pure = return
--  (<*>) = ap
