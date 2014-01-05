module Utils where

-- | Utility function for making grammatical sense in our console output.
pluralize :: Int -> String -> String
pluralize count str = (show count) ++ " " ++ (if count == 1 then str else str ++ "s")