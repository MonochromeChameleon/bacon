module Schema(createDB, seed) where

import Database.HDBC

import DatabaseConnector
import DataModel
import ORM

createDB :: IO ()
createDB = do
    schema <- readFile "bacon.sql"     -- Get our DB schema from an external SQL file
    withConnection $ createDB_ schema
    
createDB_ :: IConnection c => String -> c -> IO()
createDB_ schema conn = runRaw conn schema
    

seed :: Actor -> IO Integer
seed actor = withConnection $ seed_ actor
    
seed_ :: IConnection c => Actor -> c -> IO Integer
seed_ actor conn = do
    stmt <- prepare conn $ saveQuery actor
    execute stmt $ asSql actor