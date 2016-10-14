-- Lab03 ex. 3
module BinTree where

data BinTree a =
    Leaf a
  | Node (BinTree a) a (BinTree a)

sumTree :: BinTree Int -> Int
sumTree (Leaf x)         = x
sumTree (Node xs1 x xs2) = x + sumTree xs1 + sumTree xs2

values :: BinTree a -> [a]
values (Leaf x)         = [x]
values (Node xs1 x xs2) = x : values xs1 ++ values xs2

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf x)         = Leaf (f x)
mapTree f (Node xs1 x xs2) = Node (mapTree f xs1) (f x) (mapTree f xs2)

tree1 :: BinTree Int
tree1 = Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5))
