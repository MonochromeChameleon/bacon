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
        ["crawl", maxBacon] -> crawl (read maxBacon::Int)
        _ -> syntaxError

syntaxError :: IO()
syntaxError = putStrLn 
  "Usage: bacon [args]\n\
  \n\
  \initialize       Create and seed a database\n\
  \crawl max        Crawl from your seed reference to the specified level of connection"
