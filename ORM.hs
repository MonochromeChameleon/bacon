module ORM where

import Data.List(nub, intercalate)
import Database.HDBC

import Utils

-- | This handles the basic save/lookup functionality for our data types. I could have thought
-- | of a better name, no doubt, but I came from an OO world before...
class Eq a => Entity a where
    
    -- Basic utility properties
    entityType :: a -> EntityType
    imdbid :: a -> String
    bacon :: a -> Int
    readSql :: [SqlValue] -> a
    asSql :: a -> [SqlValue]


    -- | Saves the provided new entities, associating them with the root entity and flagging
    -- | that root as having been processed.
    processingResult :: Entity b => IConnection c => b -> [a] -> c -> IO()
    processingResult source values conn = do
        newValues <- findNew conn values
        
        saveMany conn newValues
        connect conn source newValues
        processed conn source
        
        return ()


    -- | Inserts a new instance of the given entity into the database        
    insert :: IConnection c => a -> c -> IO()
    insert entity conn = do
        stmt <- prepare conn $ saveQuery $ entityType entity
        execute stmt $ asSql entity
        return ()


-- ======================== --
-- EXCESSIVE GENERICISATION --
-- ======================== --

-- Below this point, we have (in effect) an enum to define the persistence properties of our two main classes

data EntityType = ActorType | FilmType deriving (Eq,Show)


-- | Typeclass to define the properties we need on the entity type data type as used by the entity typeclass.
-- | That sentence is convoluted enough to make me think I went too far, but I'm not going back now...
class EntityTypeDef t where
    -- Persistence properties and utilities
    tableName :: t -> String
    idColumnName :: t -> String
    idColumnName entityType = (tableName entityType) ++ "_id"
    columnNames :: t -> [String]
    allColumns :: t -> String
    allColumns et = intercalate "," $ columnNames et
    

    -- | Returns the appropriate SQL query to save a new instance of this Entity
    saveQuery :: t -> String
    saveQuery entityType = "INSERT INTO " ++ (tableName entityType) ++ " (" ++ (allColumns entityType) ++ ") VALUES (" ++ (parametrize $ columnNames entityType) ++ ")"
    
    
    -- | Builds the appropriate SQL query to update the processed flag on an entity
    updateQuery :: t -> String
    updateQuery et = "UPDATE " ++ (tableName et) ++ " SET processed = ? WHERE " ++ (idColumnName et) ++ " = ?"
    

-- | Make EntityType an instance of the EntityTypeDef TypeClass. See? SEE? It's ridiculous...
instance EntityTypeDef EntityType where    
    tableName et = case (et) of
        ActorType -> "actor"
        _ -> "film"
    columnNames et = case (et) of
        ActorType -> ["actor_id", "name", "bacon"]
        _ -> ["film_id", "title", "year", "bacon"]


---------------------
-- Utility Methods --
---------------------


-- | Saves a batch of new entities to the database    
saveMany :: Entity a => IConnection c => c -> [a] -> IO ()
saveMany conn as | length as == 0 = return ()
                 | otherwise = do
    stmt <- prepare conn (saveQuery $ entityType $ head as)
    executeMany stmt (map asSql as)
              

-- | Updates the given entity to set its "processed" flag as true.
processed :: Entity a => IConnection c => c -> a -> IO Integer
processed conn a = run conn queryAsString [toSql True, toSql $ imdbid a]
    where queryAsString = updateQuery $ entityType a
    
    
-- | For a given list of ids, returns the unique list of those IDs that are not already
-- | present in the database.
findNew :: Entity a => IConnection c => c -> [a] -> IO [a]
findNew conn as | length as == 0 = return []
                | otherwise = do
    let ids = map toSql $ map imdbid as
    -- Look up ids that are already present in the database
    let idCol = idColumnName et
    let tab   = tableName et
    let query = "SELECT " ++ idCol ++ " FROM " ++ tab ++ " WHERE " ++ idCol ++ " IN (" ++ parametrize ids ++ ")"
    res <- quickQuery' conn query ids

    let existingIds = map fromSql (map head res)
    -- Find those provided ids that aren't already present 
    let newItems = (filter (\x -> not(elem (toSql $ imdbid x) existingIds)) as)

    -- Return a unique list.
    return $ nub newItems
    where et = entityType $ head as

    
-- | Creates a link between this entity and a group of other entities. This relies on the assumption
-- | that the single entity will of one type, while the list will be of the other type. Which it will
-- | be, unless something truly insane is going on.
connect :: Entity a => Entity b => IConnection c => c -> a -> [b] -> IO()
connect conn a bs | length bs == 0 = return ()
                  | otherwise = do
    stmt <- prepare conn ("INSERT INTO actor_film (" ++ (idColumnName $ entityType a) ++ ", " ++ (idColumnName etb) ++ ") VALUES (?, ?)")
    executeMany stmt (map (\x -> [toSql $ imdbid a, toSql $ imdbid x]) bs)
    where etb = entityType $ head bs
