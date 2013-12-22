module StringUtils where

pluralize :: Int -> String -> String
pluralize count str = (show count) ++ " " ++ (if count == 1 then str else str ++ "s")