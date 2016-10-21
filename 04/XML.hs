module XML where

data XML = XText String
         | XNode String [Attr] [XML]
data Attr = String := String

foldXML :: (String -> a) -> (String -> [Attr] -> [a] -> a) -> XML -> a
foldXML f _ (XText s) = f s
foldXML _ f (XNode s as []) = f s as []
foldXML f1 f2 (XNode s as xs) = f2 s as (map (foldXML f1 f2) xs)


attrToString :: Attr -> String
attrToString (a := b) = a ++ "='" ++ b ++ "'"

xmlToString :: XML -> String
xmlToString = foldXML id
  (\ s as ss ->
    "<" ++ unwords (s : map attrToString as) ++ ">" ++ concat ss ++"</"++s++">")

xml :: XML
xml = XNode "foo" ["x" := "y"] [XNode "bar" [] [XText "test"]]
