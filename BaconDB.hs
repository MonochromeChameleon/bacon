module BaconDB where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO

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
    
    return (bacon, isIncomplete)
    
loadActorsWithBacon :: Bacon -> IO [Actor]
loadActorsWithBacon bacon = withConnection $ loadActorsWithBacon_ bacon

loadActorsWithBacon_ :: Bacon -> Connection -> IO [Actor]
loadActorsWithBacon_ bacon conn = do
    putStrLn $ "Loading actors with " ++ (pluralize bacon "slice") ++ " of bacon"
    let query = "SELECT * FROM actor WHERE bacon = ? and processed = ?"
    res <- quickQuery' conn query [toSql bacon, toSql False]
    
    return $ map readSql res
    
    
storeActors :: Film -> [Actor] -> IO ()
storeActors film actors = withConnection $ save film actors
        
loadUnprocessedFilms :: IO [Film]
loadUnprocessedFilms = withConnection loadUnprocessedFilms_

loadUnprocessedFilms_ :: Connection -> IO [Film]
loadUnprocessedFilms_ conn = do
    putStrLn "Loading unprocessed films"

    let query = "SELECT * FROM film WHERE processed = ? ORDER BY year ASC"
    res <- quickQuery' conn query [toSql False]
    
    putStrLn $ "Found " ++ (show $ length res)
    
    return $ map readSql res


storeFilms :: Actor -> [Film] -> IO()
storeFilms actor films = withConnection $ save actor films
    
deleteFilm :: Film -> IO()
deleteFilm film = withConnection $ deleteFilm_ film

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
    
