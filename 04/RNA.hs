module RNA where

data Base = U | A | C | G
  deriving Show
type RNA = [Base]

parseBase :: Char -> Maybe Base
parseBase 'U' = Just U
parseBase 'A' = Just A
parseBase 'C' = Just C
parseBase 'G' = Just G
parseBase _   = Nothing

parseRNA :: String -> Maybe RNA
parseRNA []     = Just []
parseRNA (c:cs) =
  case parseBase c of
    Nothing -> Nothing
    Just b -> case parseRNA cs of
      Nothing -> Nothing
      Just bs -> Just (b:bs)
