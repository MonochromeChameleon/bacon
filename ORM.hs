module ORM where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import StringUtils

class Eq a => Entity a where
    
    -- Basic config values
    tableName :: a -> String
    idColumnName :: a -> String
    allColumns :: a -> String
    
    saveQuery :: a -> String
    saveQuery a = "INSERT INTO " ++ (tableName a) ++ " (" ++ (allColumns a) ++ ") VALUES (" ++ (parametrize $ asSql a) ++ ")"
    
    imdbid :: a -> String
    bacon :: a -> Int
    readSql :: [SqlValue] -> a
    asSql :: a -> [SqlValue]

    processed :: Connection -> a -> IO Integer
    processed conn a = run conn ("UPDATE " ++ (tableName a) ++ " SET processed = ? WHERE " ++ (idColumnName a) ++ " = ?") [toSql True, toSql $ imdbid a]    

    saveMany :: Connection -> [a] -> IO ()
    saveMany conn as | length as == 0 = return ()
                     | otherwise = do
        stmt <- prepare conn (saveQuery $ head as)
        executeMany stmt (map asSql as)
        
    findNew :: Connection -> [a] -> IO [a]
    findNew conn as | length as == 0 = return []
                    | otherwise = do
        let ids = map toSql $ map imdbid as
        let query = "SELECT " ++ (idColumnName $ head as) ++ " FROM " ++ (tableName $ head as) ++ " WHERE " ++ (idColumnName $ head as) ++ " IN (" ++ parametrize ids ++ ")"
        res <- quickQuery' conn query ids
    
        let existingIds = map fromSql (map head res)
        let newItems = (filter (\x -> not(elem (toSql $ imdbid x) existingIds)) as)
        return $ nub newItems
        
    connect :: Entity b => Connection -> a -> [b] -> IO()
    connect conn a bs | length bs == 0 = return ()
                      | otherwise = do
        stmt <- prepare conn ("INSERT INTO actor_film (" ++ (idColumnName a) ++ ", " ++ (idColumnName $ head bs) ++ ") VALUES (?, ?)")
        executeMany stmt (map (\x -> [toSql $ imdbid a, toSql $ imdbid x]) bs)
        
    save :: Entity b => b -> [a] -> Connection -> IO()
    save source values conn = do
        newValues <- findNew conn values
        
        saveMany conn newValues
        connect conn source newValues
        processed conn source
        
        putStrLn $ "Stored " ++ (pluralize (length newValues) ("new " ++ (tableName $ head values)))


        
parametrize :: [SqlValue] -> String
parametrize values = intercalate "," $ replicate (length values) "?"
