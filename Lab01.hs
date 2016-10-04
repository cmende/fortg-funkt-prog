-- exercise01 Christoph Mende
{-# OPTIONS_GHC -w #-}

main :: IO ()
main = return ()

-- 1a
ggT :: Integer -> Integer -> Integer
ggT n m
  | m == 0    = n
  | otherwise = ggT m (mod n m)

-- 2a
kgV :: Integer -> Integer -> Integer
kgV n m = div (n * m) (ggT n m)

-- 3
fib :: Integer -> Integer
fib n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib(n - 1) + fib(n - 2)
