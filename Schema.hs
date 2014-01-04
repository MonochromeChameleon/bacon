module Schema(createDB, seed) where

import Database.HDBC

import DatabaseConnector
import ORM

createDB :: IO ()
createDB = do
    schema <- readFile "bacon.sql"     -- Get our DB schema from an external SQL file
    withConnection $ createDB_ schema


createDB_ :: IConnection c => String -> c -> IO()
createDB_ schema conn = runRaw conn schema
    

seed :: Entity a => a -> IO()
seed entity = withConnection $ insert entity