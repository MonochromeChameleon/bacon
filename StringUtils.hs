module StringUtils where


isWhiteSpace :: Char -> Bool
isWhiteSpace x = elem x [' ', '\t', '\n', '\r', '\160']



trimL :: String -> String
trimL str = dropWhile isWhiteSpace str

trimR :: String -> String
trimR str = reverse $ trimL $ reverse str

trim :: String -> String
trim = trimR.trimL

pluralize :: Int -> String -> String
pluralize count str = (show count) ++ " " ++ (if count == 1 then str else str ++ "s")