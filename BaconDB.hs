module BaconDB (getProcessingStatus, loadEntitiesWithBacon, storeProcessingResult, delete) where

import Database.HDBC

import DatabaseConnector
import DataModel
import ORM
import Schema


-- | Returns the lowest unprocessed bacon number from the actors table, along with
-- | a boolean flag indicating whether some of the actors with that bacon number 
-- | have been processed. If none of those actors have been processed, then we should
-- | begin processing from the films with bacon number one less than the queried 
-- | value.
getProcessingStatus :: IO (Bacon, Bool)
getProcessingStatus = withConnection getProcessingStatus_

getProcessingStatus_ :: IConnection c => c -> IO (Bacon, Bool)
getProcessingStatus_ conn = do
    let query = "SELECT MIN(bacon) FROM (SELECT MIN(bacon) bacon FROM actor WHERE processed = ? UNION ALL SELECT MIN(bacon) bacon FROM Film WHERE processed = ?)"
    res <- getSingleResult conn query [toSql False, toSql False]
    
    let incomplete = "SELECT COUNT(*) > 0 FROM actor WHERE bacon = ? AND processed = ?"
    incompleteRes <- getSingleResult conn incomplete [(res!!0), toSql True]
    
    let isIncomplete = (fromSql (incompleteRes!!0)::Int) == 1
    let bacon = fromSql (res!!0)::Bacon
    
    -- If the actor processing is incomplete then that is the correct bacon number 
    -- to be running. If not, then we want to check the preceding level of film instead.
    let baconToProcess = if isIncomplete then bacon else bacon - 1
    
    return (baconToProcess, isIncomplete)
    

-- | Returns a batch of up to 50,000 unprocessed entities with the given type and bacon number.
loadEntitiesWithBacon :: Entity a => EntityType -> Bacon -> IO [a]
loadEntitiesWithBacon et bacon = withConnection $ loadEntitiesWithBacon_ et bacon

loadEntitiesWithBacon_ :: IConnection c => Entity a => EntityType -> Bacon -> c -> IO [a]
loadEntitiesWithBacon_ et bacon conn = do
    let query = "SELECT " ++ (allColumns et) ++ " FROM " ++ (tableName et) ++ " WHERE bacon = ? AND processed = ? LIMIT 50000"
    res <- quickQuery' conn query [toSql bacon, toSql False]
    
    return $ map readSql res
    
    
-- | Creates entries in the actor_film table for the given source and values.
storeProcessingResult :: Entity a => Entity b => a -> [b] -> IO ()
storeProcessingResult source values = tryWithConnection $ processingResult source values



delete :: Entity a => a -> IO ()
delete et = tryWithConnection $ delete_ et

delete_ :: Entity a => IConnection c => a -> c -> IO ()
delete_ e conn = do
    run conn ("DELETE FROM " ++ (tableName et) ++ " WHERE " ++ (idColumnName et) ++ " = ?") [toSql $ imdbid e]
    run conn ("DELETE FROM actor_film WHERE " ++ (idColumnName et) ++ " = ?") [toSql $ imdbid e]
    return ()
    where et = entityType e

        
-- | Utility function to return the first value from a query.
getSingleResult :: IConnection c => c -> String -> [SqlValue] -> IO [SqlValue]
getSingleResult conn query params = do
    res <- quickQuery' conn query params
    return (res!!0)
