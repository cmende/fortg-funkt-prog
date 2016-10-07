module Lecture01 where
{-# OPTIONS_GHC -w #-}

main :: IO ()
main = return ()


-- comment

{-
comment
-}

magicNumber :: Int
magicNumber = 1
-- magicNumber = 2

i :: Integer
i = 1234123134141234123412341234213434

d :: Double
d = 1.2

typeError = fromIntegral i + d

b1 :: Bool
b1 = False

b2 :: Bool
b2 = True

c :: char
c = '\n'

ex5 = mod 4 5

ex9 = False || True

ex11 = 'a' /= 'b'

ex12 = "ab" <= "b"

ex13 = 3 + 4 * 6

ex14 = if 3 < 5 then 4 else 5

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
