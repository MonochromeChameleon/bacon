module ORM where

import Data.List(nub, intercalate)
import Database.HDBC


-- | This handles the basic save/lookup functionality for our data types. I could have thought
-- | of a better name, no doubt, but I came from an OO world before...
class Eq a => Entity a where
    
    entityType :: a -> EntityType
    imdbid :: a -> String
    bacon :: a -> Int
    readSql :: [SqlValue] -> a
    asSql :: a -> [SqlValue]


    -- Saves the provided new entities, associating them with the root entity and flagging
    -- that root as having been processed.
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



data EntityType = ActorType | FilmType deriving (Eq,Show)

class EntityTypeDef t where
    tableName :: t -> String
    idColumnName :: t -> String
    idColumnName entityType = (tableName entityType) ++ "_id"
    columnNames :: t -> [String]
    allColumns :: t -> String
    
    -- Returns the appropriate SQL query to save a new instance of this Entity
    saveQuery :: t -> String
    saveQuery entityType = "INSERT INTO " ++ (tableName entityType) ++ " (" ++ (allColumns entityType) ++ ") VALUES (" ++ (parametrize $ columnNames entityType) ++ ")"
    


instance EntityTypeDef EntityType where
    
    tableName et = case (et) of
        ActorType -> "actor"
        _ -> "film"
    columnNames et = case (et) of
        ActorType -> ["actor_id", "name", "bacon"]
        _ -> ["film_id", "title", "year", "bacon"]
    allColumns et = intercalate "," $ columnNames et
    


instance Enum EntityType where
    toEnum 0 = ActorType
    toEnum 1 = FilmType
    toEnum v = toEnum(mod v 2) -- Allow unbounded enum so that succ is defined on both values.
    
    fromEnum ActorType = 0
    fromEnum FilmType = 1


---------------------
-- Utility Methods --
---------------------


-- Saves a batch of new entities to the database    
saveMany :: Entity a => IConnection c => c -> [a] -> IO ()
saveMany conn as | length as == 0 = return ()
                 | otherwise = do
    stmt <- prepare conn (getSaveQuery as)
    executeMany stmt (map asSql as)
              
getSaveQuery :: Entity a => [a] -> String
getSaveQuery (a:as) = saveQuery $ entityType a

getUpdateQuery :: Entity a => [a] -> String
getUpdateQuery (a:as) = "UPDATE " ++ (tableName et) ++ " SET processed = ? WHERE " ++ (idColumnName et) ++ " = ?"
    where et = entityType a

-- Updates the given entity to set its "processed" flag as true.
processed :: Entity a => IConnection c => c -> a -> IO Integer
processed conn a = run conn queryAsString [toSql True, toSql $ imdbid a]
    where queryAsString = getUpdateQuery [a]
    
-- For a given list of ids, returns the unique list of those IDs that are not already
-- present in the database.
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

    
-- Creates a link between this entity and a group of other entities. This relies on the assumption
-- that the single entity will of one type, while the list will be of the other type.
connect :: Entity a => Entity b => IConnection c => c -> a -> [b] -> IO()
connect conn a bs | length bs == 0 = return ()
                  | otherwise = do
    stmt <- prepare conn ("INSERT INTO actor_film (" ++ (idColumnName $ entityType a) ++ ", " ++ (idColumnName etb) ++ ") VALUES (?, ?)")
    executeMany stmt (map (\x -> [toSql $ imdbid a, toSql $ imdbid x]) bs)
    where etb = entityType $ head bs

-- | Utility function - creates a string with the appropriate number of question marks for
-- | the arguments to a query.
parametrize :: [a] -> String
parametrize values = intercalate "," $ replicate (length values) "?"