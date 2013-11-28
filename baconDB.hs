module BaconDB where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO

import DataModel
import Schema
import StringUtils

    
getProcessingStatus :: IO (Bacon, Bool)
getProcessingStatus = do
    conn <- connectSqlite3 "bacon.db"
    let query = "SELECT MIN(bacon) FROM actor WHERE processed = ?"
    res <- getSingleResult conn query [toSql False]
    
    let incomplete = "SELECT COUNT(*) > 0 FROM actor WHERE bacon = ? AND processed = ?"
    incompleteRes <- getSingleResult conn incomplete [(res!!0), toSql True]
    
    let isIncomplete = (fromSql (incompleteRes!!0)::Int) == 1
    let bacon = fromSql (res!!0)::Bacon
    
    return (bacon, isIncomplete)
        
getSingleResult :: Connection -> String -> [SqlValue] -> IO [SqlValue]
getSingleResult conn query params = do
    res <- quickQuery' conn query params
    return (res!!0)

loadActorsWithBacon :: Bacon -> IO [Actor]
loadActorsWithBacon bacon = do
    conn <- connectSqlite3 "bacon.db"
    putStrLn $ "Loading actors with " ++ (pluralize bacon "slice") ++ " of bacon"
    let query = "SELECT * FROM actor WHERE bacon = ? and processed = ?"
    res <- quickQuery' conn query [toSql bacon, toSql False]
    commit conn
    
    return $ map convertSQLActor res
    
loadUnprocessedFilms :: IO [Film]
loadUnprocessedFilms = do
    conn <- connectSqlite3 "bacon.db"
    putStrLn "Loading unprocessed films"

    let query = "SELECT * FROM film WHERE processed = ? ORDER BY year ASC"
    res <- quickQuery' conn query [toSql False]
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
    bacon = fromSql $ sqlValues!!2 }

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
    stmt <- prepare conn "INSERT INTO actor (actor_id, name, bacon) VALUES (?, ?, ?)"
    executeMany stmt (map (\x -> [toSql $ actor_id x, toSql $ name x, toSql $ bacon x]) actors)

parametrize :: [SqlValue] -> String
parametrize values = intercalate "," $ replicate (length values) "?"