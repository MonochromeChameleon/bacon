module Utils where

import Data.List(nub, intercalate)

-- | Utility function for making grammatical sense in our console output.
pluralize :: Int -> String -> String
pluralize count str = (show count) ++ " " ++ (if count == 1 then str else str ++ "s")


-- | Utility function - creates a string with the appropriate number of question marks for
-- | the arguments to a query.
parametrize :: [a] -> String
parametrize values = intercalate "," $ replicate (length values) "?"