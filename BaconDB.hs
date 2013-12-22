module BaconDB (getProcessingStatus, loadActorsWithBacon, storeActors, loadFilmsWithBacon, storeFilms, deleteFilm) where

import Database.HDBC

import DatabaseConnector
import DataModel
import ORM
import Schema
import StringUtils


-- | Returns the lowest unprocessed bacon number from the actors table, along with
-- | a boolean flag indicating whether some of the actors with that bacon number 
-- | have been processed. If none of those actors have been processed, then we should
-- | begin processing from the films with bacon number one less than the queried 
-- | value.
getProcessingStatus :: IO (Bacon, Bool)
getProcessingStatus = withConnection getProcessingStatus_

getProcessingStatus_ :: IConnection c => c -> IO (Bacon, Bool)
getProcessingStatus_ conn = do
    let query = "SELECT MIN(bacon) FROM actor WHERE processed = ?"
    res <- getSingleResult conn query [toSql False]
    
    let incomplete = "SELECT COUNT(*) > 0 FROM actor WHERE bacon = ? AND processed = ?"
    incompleteRes <- getSingleResult conn incomplete [(res!!0), toSql True]
    
    let isIncomplete = (fromSql (incompleteRes!!0)::Int) == 1
    let bacon = fromSql (res!!0)::Bacon
    
    -- If the actor processing is incomplete then that is the correct bacon number 
    -- to be running. If not, then we want to check the preceding level of film instead.
    let baconToProcess = if isIncomplete then bacon else bacon - 1
    
    return (baconToProcess, isIncomplete)


-- | Returns a batch of up to 50,000 unprocessed actors with the given bacon number.
loadActorsWithBacon :: Bacon -> IO [Actor]
loadActorsWithBacon bacon = withConnection $ loadActorsWithBacon_ bacon

loadActorsWithBacon_ :: IConnection c => Bacon -> c -> IO [Actor]
loadActorsWithBacon_ bacon conn = do
    putStrLn $ "Loading actors with " ++ (pluralize bacon "slice") ++ " of bacon"
    let query = "SELECT " ++ (allColumns dummyActor) ++ " FROM actor WHERE bacon = ? AND processed = ? LIMIT 50000"
    res <- quickQuery' conn query [toSql bacon, toSql False]
    
    return $ map readSql res
    

-- | Creates entries in the actor_film table for the given film and actors.    
storeActors :: Film -> [Actor] -> IO ()
storeActors film actors = tryWithConnection $ save film actors


-- | Returns a batch of up to 50,000 unprocessed films with the given bacon number.
loadFilmsWithBacon :: Bacon -> IO [Film]
loadFilmsWithBacon bacon = withConnection $ loadFilmsWithBacon_ bacon

loadFilmsWithBacon_ :: IConnection c => Bacon -> c -> IO [Film]
loadFilmsWithBacon_ bacon conn = do
    putStrLn $ "Loading films with " ++ (pluralize bacon "slice") ++ " of bacon"
    let query = "SELECT " ++ (allColumns dummyFilm) ++ " FROM film WHERE bacon = ? AND processed = ? LIMIT 50000"
    res <- quickQuery' conn query [toSql bacon, toSql False]

    return $ map readSql res


-- | Creates entries in the actor_film table for the given actor and films.
storeFilms :: Actor -> [Film] -> IO()
storeFilms actor films = tryWithConnection $ save actor films
    
-- | Deletes all references to the provided film within the film and actor_film tables.
deleteFilm :: Film -> IO()
deleteFilm film = tryWithConnection $ deleteFilm_ film

deleteFilm_ :: IConnection c => Film -> c -> IO ()
deleteFilm_ film conn = do
    putStrLn "Deleting this naughty film"
    run conn "DELETE FROM film WHERE film_id = ?" [toSql $ imdbid film]
    run conn "DELETE FROM actor_film WHERE film_id = ?" [toSql $ imdbid film]
    putStrLn "All gone. Lovely."

        
-- | Utility function to return the first value from a query.
getSingleResult :: IConnection c => c -> String -> [SqlValue] -> IO [SqlValue]
getSingleResult conn query params = do
    res <- quickQuery' conn query params
    return (res!!0)
