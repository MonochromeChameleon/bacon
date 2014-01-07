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
import DataModel
import Search
import Server
import Spider

-- | Main command for runing our Bacon crawler.
main :: IO()
main = do 
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
        ["initialize"] -> do 
            seedActor <- runSearch
            case seedActor of
                Just actor -> do
                    isNew <- addConfig actor
                    if isNew then do
                        createDB
                        seed actor
                    else return ()
                _ -> return ()
        ["crawl", maxBacon] -> crawl (read maxBacon::Int)
        ["configure"] -> switchConfig
        ["server"] -> runServer
        _ -> syntaxError


-- | For unrecognized commands, we print a helpful message instead.
syntaxError :: IO()
syntaxError = putStrLn 
  "Usage: bacon [args]\n\
  \initialize       Create and seed a database\n\
  \crawl [int]      Crawl from your seed reference to the specified level of connection\n\
  \configure        Switch between available configurations\n\
  \server           Start the Bacon application on http://localhost:8000/"
