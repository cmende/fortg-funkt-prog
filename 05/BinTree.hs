module BinTree where

data BinTree a =
    Leaf a
  | Node (BinTree a) a (BinTree a)
  deriving Show

minTree :: BinTree Int -> Int
minTree (Leaf x)     = x
minTree (Node l x r) = min (min (minTree l) x) (minTree r)

replace :: BinTree a -> b -> BinTree b
replace (Leaf _) x     = Leaf x
replace (Node l _ r) x = Node (replace l x) x (replace r x)

replaceMinRec :: BinTree Int -> a -> (BinTree a,Int)
replaceMinRec (Leaf x) y     = (Leaf y,x)
replaceMinRec (Node l x r) y =
  let (x1,y1) = replaceMinRec l y
      (x2,y2) = replaceMinRec r y
  in (Node x1 y x2,min (min y1 x) y2)

replaceMin :: BinTree Int -> BinTree Int
replaceMin xs =
  let (t,m) = replaceMinRec xs m
  in t

tree1 :: BinTree Int
tree1 = Node (Leaf 1) 2 (Leaf 3)

tree2 :: BinTree Int
tree2 = Node (Leaf 2) 1 (Leaf 3)

tree3 :: BinTree Int
tree3 = Node (Leaf 3) 2 (Leaf 1)
