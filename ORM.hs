module ORM where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import StringUtils

-- | This handles the basic save/lookup functionality for our data types. I could have thought
-- | of a better name, no doubt, but I came from an OO world before...
class Eq a => Entity a where
    
    -- Basic config values
    tableName :: a -> String
    idColumnName :: a -> String
    allColumns :: a -> String
    
    imdbid :: a -> String
    bacon :: a -> Int
    readSql :: [SqlValue] -> a
    asSql :: a -> [SqlValue]
    
    ---------------------
    -- Utility Methods --
    ---------------------

    -- Returns the appropriate SQL query to save a new instance of this Entity
    saveQuery :: a -> String
    saveQuery a = "INSERT INTO " ++ (tableName a) ++ " (" ++ (allColumns a) ++ ") VALUES (" ++ (parametrize $ asSql a) ++ ")"

    -- Saves a batch of new entities to the database    
    saveMany :: Connection -> [a] -> IO ()
    saveMany conn as | length as == 0 = return ()
                     | otherwise = do
        stmt <- prepare conn query
        executeMany stmt (map asSql as)
            where query = saveQuery $ head as
    
    -- Updates the given entity to set its "processed" flag as true.
    processed :: Connection -> a -> IO Integer
    processed conn a = run conn queryAsString [toSql True, toSql $ imdbid a]
        where queryAsString = "UPDATE " ++ (tableName a) ++ " SET processed = ? WHERE " ++ (idColumnName a) ++ " = ?"
        
    -- For a given list of ids, returns the unique list of those IDs that are not already
    -- present in the database.
    findNew :: Connection -> [a] -> IO [a]
    findNew conn as | length as == 0 = return []
                    | otherwise = do
        let ids = map toSql $ map imdbid as
        -- Look up ids that are already present in the database
        let idCol = idColumnName $ head as
        let tab   = tableName $ head as        
        let query = "SELECT " ++ idCol ++ " FROM " ++ tab ++ " WHERE " ++ idCol ++ " IN (" ++ parametrize ids ++ ")"
        res <- quickQuery' conn query ids
    
        let existingIds = map fromSql (map head res)
        -- Find those provided ids that aren't already present 
        let newItems = (filter (\x -> not(elem (toSql $ imdbid x) existingIds)) as)

        -- Return a unique list.
        return $ nub newItems

        
    -- Creates a link between this entity and a group of other entities. This relies on the assumption
    -- that the single entity will of one type, while the list will be of the other type.
    connect :: Entity b => Connection -> a -> [b] -> IO()
    connect conn a bs | length bs == 0 = return ()
                      | otherwise = do
        stmt <- prepare conn ("INSERT INTO actor_film (" ++ (idColumnName a) ++ ", " ++ (idColumnName $ head bs) ++ ") VALUES (?, ?)")
        executeMany stmt (map (\x -> [toSql $ imdbid a, toSql $ imdbid x]) bs)
        
    ------------------------
    -- Non-utility method --
    ------------------------

    -- Saves the provided new entities, associating them with the root entity and flagging
    -- that root as having been processed.
    save :: Entity b => b -> [a] -> Connection -> IO()
    save source values conn = do
        newValues <- findNew conn values
        
        saveMany conn newValues
        connect conn source newValues
        processed conn source
        
        putStrLn $ "Stored " ++ (pluralize (length newValues) ("new " ++ (tableName $ head values)))


-- | Utility function - creates a string with the appropriate number of question marks for
-- | the arguments to a query.
parametrize :: [SqlValue] -> String
parametrize values = intercalate "," $ replicate (length values) "?"
