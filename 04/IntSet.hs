module IntSet where

type IntSet = Int -> Bool

empty :: IntSet
empty _ = False

isElem :: IntSet -> Int -> Bool
isElem ns = ns

singleton :: Int -> IntSet
singleton n = (== n)

insert :: Int -> IntSet -> IntSet
insert n ns x = x == n || ns x

remove :: Int -> IntSet -> IntSet
remove n ns x = x /= n && ns x

union :: IntSet -> IntSet -> IntSet
union n1s n2s x = n1s x || n2s x

intersect :: IntSet -> IntSet -> IntSet
intersect n1s n2s x = n1s x && n2s x

listToSet :: [Int] -> IntSet
listToSet [] = empty
--listToSet (n:ns) = (singleton n) `union` (listToSet ns)
listToSet ns = foldr (union . singleton) empty ns
