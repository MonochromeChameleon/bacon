import Spider

main :: IO()
main = do
    crawlActor 0 2 "http://www.imdb.com/name/nm0000102/"
    return ()
