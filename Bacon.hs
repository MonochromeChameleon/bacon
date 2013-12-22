{-
Additional required packages:
Database.HDBC           -- cabal install HDBC
Database.HDBC.Sqlite3   -- cabal install HDBC-Sqlite3
Text.HTML.TagSoup       -- cabal install TagSoup
Language.Haskell.TH.Ppr -- cabal install template-haskell
Network.HTTP.Conduit    -- cabal install http-conduit
-}



import System.Environment

import Config
import BaconDB
import Spider
import DataModel
import Schema

kevin :: Actor
kevin = Actor { name = "Kevin Bacon", actor_details = details }
    where details = IMDBDetails { imdbId = "nm0000102", baconNumber = 0 }

main :: IO()
main = do 
    args <- getArgs
    case args of
        ["initialize"] -> do 
            createDB
            seed kevin
            return ()
        ["crawl", maxBacon] -> crawl (read maxBacon::Int)
        _ -> syntaxError

syntaxError :: IO()
syntaxError = putStrLn 
  "Usage: bacon [args]\n\
  \n\
  \initialize       Create and seed a database\n\
  \crawl max        Crawl from your seed reference to the specified level of connection"
