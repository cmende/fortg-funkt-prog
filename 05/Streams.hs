module Streams where

data Stream a = Cons a (Stream a)

toList :: Stream a -> [a]
toList (Cons x xs) = x : toList xs

repeatStream :: a -> Stream a
repeatStream x = Cons x (repeatStream x)

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f (Cons x xs) = Cons (f x) (mapStream f xs)

iterateStream :: (a -> a) -> a -> Stream a
iterateStream f x = Cons x (iterateStream f (f x))

nats :: Stream Int
nats = iterateStream (+1) 0

pos :: Stream Int
pos = iterateStream (+1) 1

ruler :: Stream Int
ruler = mapStream test (iterateStream (+1) 1)

ruler2 :: Stream Int
--ruler2 = mapStream test (interleaveStreams odds evens)
--ruler2 = interleaveStreams (mapStream test odds) (mapStream test evens)
--ruler2 = interleaveStreams nulls (mapStream (+1) (mapStream test pos))
ruler2 = interleaveStreams nulls (mapStream (+1) ruler2)

test :: Int -> Int
test x = case mod x 2 of
  0 -> 1 + test (div x 2)
  _ -> 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) yys = Cons x (interleaveStreams yys xs)

nulls :: Stream Int
nulls = repeatStream 0

odds :: Stream Int
odds = iterateStream (+2) 1

evens :: Stream Int
evens = iterateStream (+2) 2
