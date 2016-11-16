module Talk where

import           Prelude hiding (filter, map)

-- Problem
doubleOdds :: [Int] -> [Int]
doubleOdds xs = map (*2) (filter odd xs)

-- Workaround
doubleOdds' :: [Int] -> [Int]
doubleOdds' = h
  where
    h []     = []
    h (x:xs) = if odd x then (x * 2) : h xs else h xs


-- Used functions
filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\ x xs -> if f x then x : xs else xs) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\ x xs -> f x : xs) []

-- (list consumer) foldr k z xs = replace cons with k, replace nil with z in xs
-- (list producer) build g = g (:) []
-- => foldr k z (build g)
-- = foldr k z (g (:) [])
-- = g k z (see foldr definition)

-- ((cons) -> nil -> List a)
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
-- build :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

-- replace : and [] with build
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xxs = build
  (\ c n -> foldr (\ x xs -> if f x then c x xs else xs) n xxs)

map' :: (a -> b) -> [a] -> [b]
map' f xxs = build
  (\ c n -> foldr (\ x xs -> c (f x) xs) n xxs)

-- replace map and filter
doubleOdds'' :: [Int] -> [Int]
doubleOdds'' xxs = build
  (\ c0 n0 -> foldr (\ x xs -> c0 (x * 2) xs) n0 (build
    (\ c1 n1 -> foldr (\ x xs -> if odd x then c1 x xs else xs) n1 xxs)))

-- foldr k z (build g) = g k z
doubleOdds''' :: [Int] -> [Int]
doubleOdds''' xxs = build
  (\ c0 n0 ->
    (\ c1 n1 -> foldr (\ x xs -> if odd x then c1 x xs else xs) n1 xxs)
      (\ x xs -> c0 (x * 2) xs) n0)

-- inline build
doubleOdds'''' :: [Int] -> [Int]
doubleOdds'''' xxs =
    (\ c1 n1 -> foldr (\ x xs -> if odd x then c1 x xs else xs) n1 xxs)
      (\ x xs -> (x * 2) : xs) []

doubleOdds''''' :: [Int] -> [Int]
doubleOdds''''' =
  foldr (\ x xs -> if odd x then (x * 2) : xs else xs) []
