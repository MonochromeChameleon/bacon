{-
Additional required packages:
Database.HDBC           -- cabal install HDBC
Database.HDBC.Sqlite3   -- cabal install HDBC-Sqlite3
Text.HTML.TagSoup       -- cabal install TagSoup
Language.Haskell.TH.Ppr -- cabal install template-haskell
Network.HTTP.Conduit    -- cabal install http-conduit
Happstack.Lite          -- cabal install happstack-lite
-}



import System.Environment
import System.IO

import Config
import BaconDB
import Spider
import DataModel
import Search

main :: IO()
main = do 
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
        ["initialize"] -> do 
            seedActor <- runSearch
            case seedActor of
                Just actor -> do
                    addConfig actor
                    createDB
                    seed actor
                _ -> return ()
        ["crawl", maxBacon] -> crawl (read maxBacon::Int)
        _ -> syntaxError

syntaxError :: IO()
syntaxError = putStrLn 
  "Usage: bacon [args]\n\
  \initialize       Create and seed a database\n\
  \crawl max        Crawl from your seed reference to the specified level of connection"
