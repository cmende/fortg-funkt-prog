module Lab08 where

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
  --(_,y) >>= k = k y
  x >>= k = k (snd x)

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
-- cross xs ys = xs >>= (\ x -> ys >>= (\ y -> [(x,y)]))
cross xs ys = do
  x <- xs
  y <- ys
  return (x,y)
