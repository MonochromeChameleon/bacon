module Schema where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO

import ORM
import DataModel

----QQQQ Insert statements should use ORM props and shared connection bits

createDB :: IO ()
createDB = do
    schema <- readFile "bacon.sql"     -- Get our DB schema from an external SQL file
    conn <- connectSqlite3 "bacon.db"
    runMany conn $ schemaParts schema -- Run all individual schema commands against the connection
    commit conn
    

seed :: Actor -> IO ()
seed actor = do
    conn <- connectSqlite3 "bacon.db"
    stmt <- prepare conn "INSERT INTO actor (actor_id, name, bacon) VALUES (?, ?, ?)"
    execute stmt [toSql $ imdbid actor, toSql $ name actor, toSql $ bacon actor]
    commit conn


-- Break the Schema file into individual statements separated by semi-colons. This is safe as 
-- long as the schema file doesn't contain any bootstrap data that happens to contain semi-colons
-- inside string values etc.
schemaParts :: String -> [String]
schemaParts = doSchemaParts [] -- call through to curried recursive function

-- Recursive function for collecting schema componenets
doSchemaParts :: [String] -> String -> [String]
doSchemaParts parts [] = parts
doSchemaParts parts schema = doSchemaParts (parts ++ [part ++ ";"]) (tail rest)
    where (part, rest) = span (\x -> x /= ';') schema

-- Executes multiple commands
runMany :: Connection -> [String] -> IO()
runMany conn [] = return ()
runMany conn (part:parts) = do
    run conn part []
    runMany conn parts
    return ()
