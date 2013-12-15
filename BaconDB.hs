module BaconDB where

import Control.Concurrent
import Control.Exception
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO
import System.Random

import DataModel
import ORM
import Schema
import StringUtils


-- |Wrapper for functions that require a connection to the database - any externally-exposed
-- |functions in this module that use a db connection should simply defer executrion to a 
-- |private function with the connection as its last argument, to be executed by this wrapper 
-- |function. In turn, that means that we only have a single location in which the connection 
-- |is ever defined, and we can be confident that it will never be left open.
withConnection :: (Connection -> IO a) -> IO a
withConnection func = do
    conn <- connectSqlite3 "bacon.db"
    res <- func conn
    commit conn
    disconnect conn
    return res


-- | Write-only db connection handler that will handle a locked database and try again after 1 second
tryWithConnection :: (Connection -> IO ()) -> IO ()
tryWithConnection func = do
    result <- try (withConnection func) :: IO (Either SqlError ())
    case result of
        Left r -> tryAgain func
        Right _ -> return ()
        
tryAgain :: (Connection -> IO ()) -> IO ()
tryAgain func = do
    putStrLn "Contention for DB Connection - waiting"
    randomDelay <- randomRIO(100000, 10000000) -- 0.1 to 10 seconds
    threadDelay randomDelay
    tryWithConnection func

    
getProcessingStatus :: IO (Bacon, Bool)
getProcessingStatus = withConnection getProcessingStatus_

getProcessingStatus_ :: Connection -> IO (Bacon, Bool)
getProcessingStatus_ conn = do
    let query = "SELECT MIN(bacon) FROM actor WHERE processed = ?"
    res <- getSingleResult conn query [toSql False]
    
    let incomplete = "SELECT COUNT(*) > 0 FROM actor WHERE bacon = ? AND processed = ?"
    incompleteRes <- getSingleResult conn incomplete [(res!!0), toSql True]
    
    let isIncomplete = (fromSql (incompleteRes!!0)::Int) == 1
    let bacon = fromSql (res!!0)::Bacon
    
    -- If the actor processing is incomplete then that is the correct bacon number to be running. If not,
    -- then we want to check the preceding level of film instead.
    let baconToProcess = if isIncomplete then bacon else bacon - 1
    
    return (baconToProcess, isIncomplete)


loadActorsWithBacon :: Bacon -> IO [Actor]
loadActorsWithBacon bacon = withConnection $ loadActorsWithBacon_ bacon

loadActorsWithBacon_ :: Bacon -> Connection -> IO [Actor]
loadActorsWithBacon_ bacon conn = do
    putStrLn $ "Loading actors with " ++ (pluralize bacon "slice") ++ " of bacon"
    let query = "SELECT " ++ (allColumns dummyActor) ++ " FROM actor WHERE bacon = ? AND processed = ? LIMIT 50000"
    res <- quickQuery' conn query [toSql bacon, toSql False]
    
    return $ map readSql res
    
    
storeActors :: Film -> [Actor] -> IO ()
storeActors film actors = tryWithConnection $ save film actors



loadFilmsWithBacon :: Bacon -> IO [Film]
loadFilmsWithBacon bacon = withConnection $ loadFilmsWithBacon_ bacon

loadFilmsWithBacon_ :: Bacon -> Connection -> IO [Film]
loadFilmsWithBacon_ bacon conn = do
    putStrLn $ "Loading films with " ++ (pluralize bacon "slice") ++ " of bacon"
    let query = "SELECT " ++ (allColumns dummyFilm) ++ " FROM film WHERE bacon = ? AND processed = ? LIMIT 50000"
    res <- quickQuery' conn query [toSql bacon, toSql False]

    return $ map readSql res



        
loadUnprocessedFilms :: IO [Film]
loadUnprocessedFilms = withConnection loadUnprocessedFilms_

loadUnprocessedFilms_ :: Connection -> IO [Film]
loadUnprocessedFilms_ conn = do
    putStrLn "Loading unprocessed films"

    let query = "SELECT " ++ (allColumns dummyFilm) ++ " FROM film WHERE processed = ? ORDER BY year ASC"
    res <- quickQuery' conn query [toSql False]
    
    putStrLn $ "Found " ++ (show $ length res)
    
    return $ map readSql res


storeFilms :: Actor -> [Film] -> IO()
storeFilms actor films = tryWithConnection $ save actor films
    
deleteFilm :: Film -> IO()
deleteFilm film = tryWithConnection $ deleteFilm_ film

deleteFilm_ :: Film -> Connection -> IO ()
deleteFilm_ film conn = do
    putStrLn "Deleting this naughty film"
    run conn "DELETE FROM film WHERE film_id = ?" [toSql $ imdbid film]
    run conn "DELETE FROM actor_film WHERE film_id = ?" [toSql $ imdbid film]
    putStrLn "All gone. Lovely."

        
getSingleResult :: Connection -> String -> [SqlValue] -> IO [SqlValue]
getSingleResult conn query params = do
    res <- quickQuery' conn query params
    return (res!!0)
    
