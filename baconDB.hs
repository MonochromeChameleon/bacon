module BaconDB where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO

import DataModel

createDB :: IO ()
createDB = do
    schema <- readFile "bacon.sql"     -- Get our DB schema from an external SQL file
    conn <- connectSqlite3 "bacon.db"
    runMany conn $ schemaParts schema -- Run all individual schema commands against the connection
    commit conn

-- Break the Schema file into individual statements separated by semi-colons. This is safe as 
-- long as the schema file doesn't contain any bootstrap data that happens to contain semi-colons
-- inside string values etc.
schemaParts :: String -> [String]
schemaParts = doSchemaParts [] -- call through to curried recursive function

-- Recursive function for collecting schema componenets
doSchemaParts :: [String] -> String -> [String]
doSchemaParts parts [] = parts
doSchemaParts parts schema = doSchemaParts (parts ++ [part ++ ";"]) (tail rest)
    where (part, rest) = span (\x -> x /= ';') schema

-- Executes multiple commands
runMany :: Connection -> [String] -> IO()
runMany conn [] = return ()
runMany conn (part:parts) = do
    run conn part []
    runMany conn parts
    return ()

loadActorsWithBacon :: BaconNumber -> IO [Actor]
loadActorsWithBacon baconNumber = do
    conn <- connectSqlite3 "bacon.db"
    putStrLn $ "Loading actors with bacon level " ++ (show baconNumber)
    let query = "SELECT * FROM actor WHERE baconNumber = ? and processed = ?"
    res <- quickQuery' conn query [toSql baconNumber, toSql False]
    commit conn
    
    return $ map convertSQLActor res
    
loadFilmsWithBacon :: BaconNumber -> IO [Film]
loadFilmsWithBacon baconNumber = do
    conn <- connectSqlite3 "bacon.db"
    putStrLn $ "Loading films with bacon level " ++ (show baconNumber)
    let query = "SELECT * FROM film WHERE baconNumber = ? and processed = ? ORDER BY year ASC"
    res <- quickQuery' conn query [toSql baconNumber, toSql False]
    commit conn
    
    putStrLn $ "Found " ++ (show $ length res)
    
    return $ map convertSQLFilm res


convertSQLFilm :: [SqlValue] -> Film
convertSQLFilm sqlValues = Film {
    film_id = fromSql $ sqlValues!!0,
    title = fromSql $ sqlValues!!1,
    year = fromSql $ sqlValues!!2 }
    
convertSQLActor :: [SqlValue] -> Actor
convertSQLActor sqlValues = Actor {
    actor_id = fromSql $ sqlValues!!0,
    name = fromSql $ sqlValues!!1,
    baconNumber = fromSql $ sqlValues!!2 }

storeFilms :: Actor -> [Film] -> IO [Film]
storeFilms actor films = do
    conn <- connectSqlite3 "bacon.db"
    putStrLn "Trying to store films"
    newFilms <- filterFilms conn films
    doStoreFilms conn newFilms
    putStrLn "Stored films"
    
    doStoreActorFilms conn actor newFilms
    putStrLn "Stored actor-film links"
    
    doMarkActorAsProcessed conn actor
    
    commit conn
    
    return newFilms
    
deleteFilm :: Film -> IO ()
deleteFilm film = do
    conn <- connectSqlite3 "bacon.db"
    putStrLn "Deleting this naughty film"
    stmt1 <- prepare conn "DELETE FROM film WHERE film_id = ?"
    execute stmt1 [toSql $ film_id film]
    
    putStrLn "That's better, now to remove any indication that anyone was ever in the film"
    stmt2 <- prepare conn "DELETE FROM actor_film WHERE film_id = ?"
    execute stmt2 [toSql $ film_id film]
    
    putStrLn "All gone. Lovely."
    
    commit conn
    
    return ()
    
doStoreActorFilms :: Connection -> Actor -> [Film] -> IO()
doStoreActorFilms conn actor films = do
    stmt <- prepare conn "INSERT INTO actor_film (actor_id, film_id) VALUES (?, ?)"
    executeMany stmt (map (\x -> [toSql $ actor_id actor, toSql $ film_id x]) films)

doMarkActorAsProcessed :: Connection -> Actor -> IO()
doMarkActorAsProcessed conn actor = do
    stmt <- prepare conn "UPDATE actor SET processed = ? WHERE actor_id = ?"
    execute stmt [toSql True, toSql $ actor_id actor]
    return ()
    
doMarkFilmAsProcessed :: Connection -> Film -> IO()
doMarkFilmAsProcessed conn film = do
    stmt <- prepare conn "UPDATE film SET processed = ? WHERE film_id = ?"
    execute stmt [toSql True, toSql $ film_id film]
    return ()
    
doStoreFilmActors :: Connection -> Film -> [Actor] -> IO()
doStoreFilmActors conn film actors = do
    stmt <- prepare conn "INSERT INTO actor_film (actor_id, film_id) VALUES (?, ?)"
    executeMany stmt (map (\x -> [toSql $ actor_id x, toSql $ film_id film]) actors)
    
filterFilms :: Connection -> [Film] -> IO [Film]
filterFilms conn films = do
    let filmIds = map toSql $ map film_id films
    let query = "SELECT film_id FROM film WHERE film_id IN (" ++ parametrize filmIds ++ ")"
    res <- quickQuery' conn query filmIds
    
    let existingIds = map fromSql (map head res)
    
    let newFilms = (filter (\x -> not(elem (toSql $ film_id x) existingIds)) films)
    let uniqueNewFilms = nub newFilms
    
    return uniqueNewFilms
    
doStoreFilms :: Connection -> [Film] -> IO()
doStoreFilms conn films = do
    stmt <- prepare conn "INSERT INTO film (film_id, title, year) VALUES (?, ?, ?)"
    executeMany stmt (map (\x -> [toSql $ film_id x, toSql $ title x, toSql $ year x]) films)
    
storeActors :: Film -> [Actor] -> IO [Actor]
storeActors film actors = do
    conn <- connectSqlite3 "bacon.db"
    putStrLn "Trying to store actors"
    newActors <- filterActors conn actors
    doStoreActors conn newActors
    putStrLn "Stored actors"
    
    doStoreFilmActors conn film newActors
    putStrLn "Stored actor-film links"
    
    doMarkFilmAsProcessed conn film
    
    commit conn
    
    return newActors
    
filterActors :: Connection -> [Actor] -> IO [Actor]
filterActors conn actors = do
    let actorIds = map toSql $ map actor_id actors
    let query = "SELECT actor_id FROM actor WHERE actor_id IN (" ++ parametrize actorIds ++ ")"
    res <- quickQuery' conn query actorIds
    
    let existingIds = map fromSql (map head res)
    let newActors = (filter (\x -> not(elem (toSql $ actor_id x) existingIds)) actors)
    let uniqueNewActors = nub newActors
    
    return uniqueNewActors
    
doStoreActors :: Connection -> [Actor] -> IO()
doStoreActors conn actors = do
    stmt <- prepare conn "INSERT INTO actor (actor_id, name, baconNumber) VALUES (?, ?, ?)"
    executeMany stmt (map (\x -> [toSql $ actor_id x, toSql $ name x, toSql $ baconNumber x]) actors)

parametrize :: [SqlValue] -> String
parametrize values = intercalate "," $ replicate (length values) "?"

    
{-storeURLs :: [URL] -> IO ()
storeURLs [] = return ()
storeURLs xs =
     do conn <- connectSqlite3 "urls.db"
        stmt <- prepare conn "INSERT INTO urls (url) VALUES (?)"
        executeMany stmt (map (\x -> [toSql x]) xs)
        commit conn        

printURLs :: IO ()
printURLs = do urls <- getURLs
               mapM_ print urls

getURLs :: IO [URL]
getURLs = do conn <- connectSqlite3 "urls.db"
             res <- quickQuery' conn "SELECT url FROM urls" []
             return $ map fromSql (map head res)
             
unfoldDB :: IO ()
unfoldDB = do urls <- getURLs
              process urls

process :: [URL] -> IO ()
process [] = return ()
process (x:xs) = do print $ "Processing : " ++ x
                    urlContent <- downloadURL x
                    storeURLs (parseURLs urlContent)
                    process xs
-}