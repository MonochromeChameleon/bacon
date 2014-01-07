module BaconDB (createDB, seed, loadEntitiesWithBacon, storeProcessingResult, delete, lookupActor, lookupFilm) where

import Database.HDBC

import DatabaseConnector
import DataModel
import ORM


-- | Creates a new database using the provided schema file.
createDB :: IO ()
createDB = do
    schema <- readFile "bacon.sql"     -- Get our DB schema from an external SQL file
    withConnection $ createDB_ schema

-- | Internal function called via DatabaseConnector.withConnection to create the database
createDB_ :: IConnection c => String -> c -> IO()
createDB_ schema conn = runRaw conn schema
    

-- | Seeds the database with the given entity
seed :: Entity a => a -> IO()
seed entity = withConnection $ insert entity
    

-- | Returns a batch of up to 50,000 unprocessed entities with the given type and bacon number.
loadEntitiesWithBacon :: Entity a => EntityType -> Bacon -> IO [a]
loadEntitiesWithBacon et bacon = withConnection $ loadEntitiesWithBacon_ et bacon


-- | Internal function called via DatabaseConnector.withConnection to fetch a batch of entities.
loadEntitiesWithBacon_ :: IConnection c => Entity a => EntityType -> Bacon -> c -> IO [a]
loadEntitiesWithBacon_ et bacon conn = do
    let query = "SELECT " ++ (allColumns et) ++ " FROM " ++ (tableName et) ++ " WHERE bacon = ? AND processed = ? LIMIT 50000"
    res <- quickQuery' conn query [toSql bacon, toSql False]
    return $ map readSql res
    
    
-- | Creates entries in the actor_film table for the given source and values.
storeProcessingResult :: Entity a => Entity b => a -> [b] -> IO ()
storeProcessingResult source values = tryWithConnection $ processingResult source values


-- | Deletes the given entity and its corresponding actor_film entries
delete :: Entity a => a -> IO ()
delete et = tryWithConnection $ delete_ et


-- | Internal function called via DatabaseConnector.withConnection to delete entities
delete_ :: Entity a => IConnection c => a -> c -> IO ()
delete_ e conn = do
    run conn ("DELETE FROM " ++ (tableName et) ++ " WHERE " ++ (idColumnName et) ++ " = ?") [toSql $ imdbid e]
    run conn ("DELETE FROM actor_film WHERE " ++ (idColumnName et) ++ " = ?") [toSql $ imdbid e]
    return ()
    where et = entityType e

lookupActor :: ImdbID -> IO Actor
lookupActor imdbId = lookupEntity ActorType imdbId

lookupFilm :: ImdbID -> IO Film
lookupFilm imdbId = lookupEntity FilmType imdbId


-- | Lookup an entity by imdb id
lookupEntity :: Entity e => EntityType -> ImdbID -> IO e
lookupEntity et imdbId = withConnection $ lookup_ et imdbId


-- | Internal function called via DatabaseConnector.withConnection to lookup entities
lookup_ :: Entity e => IConnection c => EntityType -> ImdbID -> c -> IO e
lookup_ et imdbId conn = do
    let query = "SELECT " ++ (allColumns et) ++ " FROM " ++ (tableName et) ++ " WHERE " ++ (idColumnName et) ++ " = ?"
    res <- quickQuery' conn query [toSql imdbId]
    return $ readSql $ res!!0

