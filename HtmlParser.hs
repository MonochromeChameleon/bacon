module HtmlParser where

notWhiteSpace :: Char -> Bool
notWhiteSpace x = not(x == ' ' || x == '\t' || x == '\r' || x == '\n')

notOpenTag :: Char -> Bool
notOpenTag x = x /= '<'

notCloseTag :: Char -> Bool
notCloseTag x = x /= '>'

startingTagName :: String -> String
startingTagName [] = ""
startingTagName ('<':xs) = takeWhile notWhiteSpace $ takeWhile notCloseTag xs
startingTagName (_:_) = ""

stringFrom :: String -> String -> String
stringFrom _ [] = ""
stringFrom substring (x:xs) | startsWith substring (x:xs) = (x:xs)
                            | xs == [] = ""
                            | otherwise = stringFrom substring xs

stringTo :: String -> String -> String
stringTo _ [] = ""
stringTo substring str = doStringTo "" substring str

doStringTo :: String -> String -> String -> String
doStringTo builder substring (x:xs) | startsWith substring (x:xs) = builder
                                    | xs == [] = builder ++ [x]
                                    | otherwise = doStringTo (builder ++ [x]) substring xs

startsWith :: String -> String -> Bool
startsWith _ [] = False
startsWith [] _ = True
startsWith substring str = take (length substring) str == substring

splitString :: String -> String -> (String, String)
splitString _ [] = ("", "")
splitString substring str = doSplitString "" substring str

doSplitString :: String -> String -> String -> (String, String)
doSplitString before substring (x:xs) | startsWith substring (x:xs) = (before, x:xs)
                                      | otherwise = doSplitString (before ++ [x]) substring xs

extractNode :: String -> String
extractNode str = magic 0 (startingTagName str) str

magic :: Int -> String -> String -> String
magic depth tagName string = "hello"
