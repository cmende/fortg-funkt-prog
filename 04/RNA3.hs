module RNA3 where

import RNA (Base(..), RNA)

parseBase :: Char -> Either String Base
parseBase 'U' = Right U
parseBase 'A' = Right A
parseBase 'C' = Right C
parseBase 'G' = Right G
parseBase x = Left ("No base: " ++ [x])

foldEither :: (a -> b) -> Either b a -> b
foldEither _ (Left x)  = x
foldEither f (Right x) = f x

(>>-) :: Either c a -> (a -> Either c b) -> Either c b
(>>-) (Right x) f = foldEither f (Right x)
(>>-) (Left x) _  = Left x

result :: a -> Either c a
result = Right

parseRNA :: String -> Either String RNA
parseRNA []     = result []
parseRNA (c:cs) = parseRNA cs >>- (\ x -> parseBase c >>- (\ y -> result (y:x)))
