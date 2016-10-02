{-# OPTIONS_GHC -w #-}

main :: IO ()
main = return ()


-- comment

{-
comment
-}

magicNumber :: Int
magicNumber = 1

-- Input -> Output
fac :: Integer -> Integer
fac 0 = 1
fac n = fac (n - 1) * n

even :: Integer -> Bool
even n
  | n `mod` 2 == 0 = True
  | otherwise      = False

even2 :: Integer -> Bool
even2 n = mod n 2 == 0

f :: Integer -> Integer -> Integer
f x y = x+y
