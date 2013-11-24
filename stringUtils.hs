module StringUtils where

notWhiteSpace :: Char -> Bool
notWhiteSpace = not.isWhiteSpace

isWhiteSpace :: Char -> Bool
isWhiteSpace x = (x == ' ' || x == '\t' || x == '\r' || x == '\n')

trimL :: String -> String
trimL str = dropWhile isWhiteSpace str

trimR :: String -> String
trimR str = reverse $ trimL $ reverse str


trim :: String -> String
trim = trimR.trimL


startsWith :: String -> String -> Bool
startsWith _ [] = False
startsWith [] _ = True
startsWith substring str = take (length substring) str == substring


endsWith :: String -> String -> Bool
endsWith _ [] = False
endsWith [] _ = True
endsWith substring str = drop (length str - length substring) str == substring


splitString :: String -> String -> (String, String)
splitString _ [] = ("", "")
splitString substring str = doSplitString "" substring str


doSplitString :: String -> String -> String -> (String, String)
doSplitString before substring (x:xs) | startsWith substring (x:xs) = (before, x:xs)
                                      | otherwise = doSplitString (before ++ [x]) substring xs



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

