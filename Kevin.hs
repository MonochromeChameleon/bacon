import System.Environment

import BaconDB
import Spider
import DataModel
import Schema

kevin :: Actor
kevin = Actor { actor_id = "nm0000102", name = "Kevin Bacon", bacon = 0 }

main :: IO()
main = do 
    args <- getArgs
    case args of
        ["create"] -> createDB	
        ["seed"] -> seed kevin
        ["crawl", maxBacon] -> crawl (read maxBacon::Int)
        _ -> syntaxError

syntaxError :: IO()
syntaxError = putStrLn 
  "Usage: Crawler command [args]\n\
  \\n\
  \create           Create database urls.db\n\
  \show url         Shows contents of given URL\n\
  \saved            List urls on database\n\
  \crawl url        Gather urls and store in database\n\
  \unfold           Crawl each of the saved URLs\n"
