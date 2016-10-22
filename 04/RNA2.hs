module RNA2 where

import RNA hiding (parseRNA)

foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe e _ Nothing  = e
foldMaybe _ f (Just x) = f x

(>>-) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>-) x f = foldMaybe Nothing f x

result :: a -> Maybe a
result = Just

parseRNA :: String -> Maybe RNA
parseRNA []     = Just []
parseRNA (c:cs) = parseBase c >>- (\ x -> Just [x]) -- parseRNA cs
