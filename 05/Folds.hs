module Folds where

list1 :: [Int]
list1 = [1,2,3,4]

list2 :: [Int]
list2 = [9,8,7,6]

-- foldr :: (a -> b -> b) -> b -> [a] -> b

mapr :: (a -> b) -> [a] -> [b]
mapr f = foldr (\ y ys -> f y : ys) []

filterr :: (a -> Bool) -> [a] -> [a]
filterr p = foldr (\ x xs -> if p x then x : xs else xs) []

reverser :: [a] -> [a]
reverser = foldr (\ x xs -> xs ++ [x]) []

concatr :: [a] -> [a] -> [a]
concatr xs ys = foldr (:) ys xs

mapl :: (a -> b) -> [a] -> [b]
mapl f = foldl (\ ys y -> ys ++ [f y]) []

filterl :: (a -> Bool) -> [a] -> [a]
filterl p = foldl (\ xs x -> if p x then xs ++ [x] else xs) []

reversel :: [a] -> [a]
--reversel = foldl (\ xs x -> x : xs) []
reversel = foldl (flip (:)) []

concatl :: [a] -> [a] -> [a]
concatl = foldl (\ zs z -> zs ++ [z])
