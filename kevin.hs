import Spider

import DataModel

kevin = Actor { actor_id = "nm0000102", name = "Kevin Bacon", baconNumber = 0 }

main :: IO()
main = do
    crawl 0 2
    return ()
