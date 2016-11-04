module Lab06 where

import           Prelude hiding (all, any, concat, product, sum)

-- 1

class Appendable a where
  (<>) :: a -> a -> a
  empty :: a

instance Appendable [a] where
  (<>) = (++)
  empty = []

foldMapList :: Appendable m => (a -> m) -> [a] -> m
foldMapList _ []     = empty
foldMapList f (x:xs) = f x <> foldMapList f xs

mcombine :: Appendable m => [m] -> m
mcombine = foldMapList id

concat :: [[a]] -> [a]
concat = mcombine


-- 2

data Any = Any { getAny :: Bool }
  deriving Show

data All = All { getAll :: Bool }
  deriving Show

instance Appendable Any where
  Any x <> Any y = Any (x || y)
  empty = Any False

instance Appendable All where
  All x <> All y = All (x && y)
  empty = All True

all :: (a -> Bool) -> [a] -> Bool
all f = getAll . foldMapList (All . f)

any :: (a -> Bool) -> [a] -> Bool
any f = getAny . foldMapList (Any . f)

data Sum a = Sum { getSum :: a }

data Product a = Product { getProduct :: a }

instance Num a => Appendable (Sum a) where
  (Sum x) <> (Sum y) = Sum (x + y)
  empty = Sum 0

instance Num a => Appendable (Product a) where
  (Product x) <> (Product y) = Product (x * y)
  empty = Product 1

sum :: Num a => [a] -> a
sum = getSum . foldMapList Sum

product :: Num a => [a] -> a
product = getProduct . foldMapList Product

instance (Appendable a, Appendable b) => Appendable (a, b) where
  (x1,y1) <> (x2,y2) = (x1 <> x2, y1 <> y2)
  empty = (empty,empty)

sumProduct :: Num a => [a] -> (a, a)
sumProduct xs =
  let (s, p) = foldMapList (\ x -> (Sum x, Product x)) xs
  in (getSum s, getProduct p)


-- 3

data Tree a =
    Empty
  | Node (Tree a) a (Tree a)
  deriving Show


foldMapTree :: Appendable m => (a -> m) -> Tree a -> m
foldMapTree _ Empty          = empty
foldMapTree f (Node t1 x t2) = foldMapTree f t1 <> f x <> foldMapTree f t2

toList :: Tree a -> [a]
toList = foldMapTree (: [])


-- 4

data First a = First { getFirst :: Maybe a }

instance Appendable (First a) where
  First (Just x) <> _ = First (Just x)
  First Nothing <> x  = x
  empty = First Nothing

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst p = getFirst . foldMapList (First . select)
  where
    select x = if p x then Just x else Nothing

data Last a = Last { getLast :: Maybe a}

instance Appendable (Last a) where
  Last _ <> Last (Just x) = Last (Just x)
  x <> Last Nothing  = x
  empty = Last Nothing

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast p = getLast . foldMapList (Last . select)
  where select x = if p x then Just x else Nothing

-- test

list1 :: [Int]
list1 = [1,2,3]

list2 :: [Int]
list2 = [4,5,6]

list3 :: [[Int]]
list3 = [list1,list2]

list4 :: [Int]
list4 = [2,4,6]

list5 :: [Int]
list5 = [1,2,3,4,5,6,7,8,9]

list6 :: [Int]
list6 = [2,4,6,8,10,12,13]

tree1 :: Tree Int
tree1 = Node (Node Empty 1 Empty) 2 (Node (Node Empty 4 Empty) 3 Empty)
