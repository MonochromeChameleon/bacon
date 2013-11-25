import Spider

import DataModel

kevin = ImdbRecord { imdbId = "nm0000102", name = "Kevin Bacon", baconNumber = 0 }

main :: IO()
main = do
    crawl 0 1
    return ()
