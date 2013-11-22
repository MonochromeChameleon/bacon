import Control.Monad
import System.IO

import ActorParser

main :: IO()
main = do
    kevin <-readFile "kevin.txt"
    putStrLn $ extractFilmography kevin

