module XML where

data XML = XText String
         | XNode String [Attr] [XML]
data Attr = String := String

-- foldXML :: (String -> a) -> (String -> [Attr] -> [a] -> a) -> XML -> a
-- foldXML 

attrToString :: Attr -> String
attrToString (a := b) = a ++ "='" ++ b ++ "'"

xmlToString :: XML -> String
xmlToString (XText x)            = x
xmlToString (XNode x attrs xmls) =
  "<"++ unwords (x : map attrToString attrs) ++">"
  ++ concatMap xmlToString xmls ++"</"++x++">"

xml :: XML
xml = XNode "foo" ["x" := "y"] [XNode "bar" [] [XText "test"]]
