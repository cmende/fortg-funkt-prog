-- Lab03 ex. 3
module Ex3 where

data BinTree a =
    Empty
  | Node a (BinTree a)

sumTree :: BinTree Int -> Int
sumTree Empty          = 0
sumTree (Node x xs) = x + sumTree xs

values :: BinTree a -> [a]
values Empty       = []
values (Node x xs) = x : values xs

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree _ Empty       = Empty
mapTree f (Node x xs) = Node (f x) (mapTree f xs)

tree1 :: BinTree Int
tree1 = Node 1 (Node 2 (Node 3 Empty))
