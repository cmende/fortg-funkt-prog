module Lecture05 where

import           Prelude hiding (foldl, foldr, head, (++))

f1 :: Int -> Int
f1 x = x

g :: Int -> (Int -> Int)
g x y = x+y

a :: Int
a = f1 1

b :: Int -> Int
b = g 1
-- b y = 1+y

c :: Int
c = b 2

-- f :: T -> (R -> Q)
-- x :: T
-- y :: R
-- (f x) y :: Q

add :: (Int,Int) -> Int
add (x,y) = x+y

d1 :: Int -> Int -> Int
d1 = g

-- d2 :: Int -> Int -> Int
-- d2 x = g x

-- d :: Int -> Int -> Int
-- d x y = g x y

-- foldr f e [a,b,c]
-- = foldr f e (a : (b : (c : [])))
-- = (a `f` (b `f` (c `f` e)))
-- = (f a (f b (f c e)))

-- foldr (+) 0 [1,2,3]
-- = foldr (+) 0 (1 : (2 : (3 : 0)))
-- = foldr (1 + (2 + (3 + 0)))

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ e []     = e
foldr f e (x:xs) = f x (foldr f e xs)

-- foldl f e [a,b,c]
-- = foldl f e (a : (b : (c : [])))
-- = foldl f (f e a) (b : (c : []))
-- = foldl f (f (f e a) b) (c : [])
-- = foldl f (f (f (f e a) b) c) []
-- = f (f (f (f e a) b) c)


foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ e []     = e
foldl f e (x:xs) = foldl f (f e x) xs


head :: [a] -> a
head []    = error "bad head"
head (x:_) = x

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

append :: [a] -> [a] -> [a]
append [] ys     = ys
append (x:xs) ys = x : append xs ys

-- head ([1,2] ++ [3,4])
-- = head ((1:2:[]) ++ (3:4:[]))
-- = head (1 : ((2:[]) ++ (3:4:[])))
-- = head (1 : (2 : ([] ++ (3:4:[]))))
-- = head (1 : (2 : (3:4:[])))
-- = 1

-- head ([1,2] ++ [3,4])
-- = head ((1:2:[]) ++ (3:4:[]))
-- = head (1 : ((2:[]) ++ (3:4:[])))
-- = 1

loop :: a
loop = loop

test :: Int
test = const 1 loop
