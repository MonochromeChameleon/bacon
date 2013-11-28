module StringUtils where



isWhiteSpace :: Char -> Bool
isWhiteSpace x = elem x [' ', '\t', '\n', '\r', '\160']



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


pluralize :: Int -> String -> String
pluralize count str = (show count) ++ " " ++ (if count == 1 then str else str ++ "s")