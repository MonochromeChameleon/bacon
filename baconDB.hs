module BaconDB where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO

import DataModel

-- Print out multiple lines to the console
putLines :: [String] -> IO()
putLines [] = return ()
putLines (line:lines) = do
    putStrLn line
    putLines lines

createDB :: IO ()
createDB = do
    schema <- readFile "bacon.sql"     -- Get our DB schema from an external SQL file
    conn <- connectSqlite3 "bacon.db"
    runMany conn $ schemaParts schema -- Run all individual schema commands against the connection
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



storeFilms :: [Film] -> IO [Film]
storeFilms films = do
    conn <- connectSqlite3 "bacon.db"
    putStrLn "Trying to store films"
    newFilms <- filterFilms conn films
    putStrLn "Filtered films"
    doStoreFilms conn newFilms
    putStrLn "Stored"
    return newFilms
    
filterFilms :: Connection -> [Film] -> IO [Film]
filterFilms conn films = do
    let filmIds = map toSql $ map imdbId films
    let query = "SELECT imdbId FROM film WHERE imdbId IN (" ++ parametrize filmIds ++ ")"
    res <- quickQuery' conn query filmIds
    
    let existingIds = map fromSql (map head res)
    
    return (filter (\x -> not(elem (toSql $ imdbId x) existingIds)) films)
    
doStoreFilms :: Connection -> [Film] -> IO()
doStoreFilms conn films = do
    stmt <- prepare conn "INSERT INTO film (imdbId, name, baconNumber) VALUES (?, ?, ?)"
    executeMany stmt (map (\x -> [toSql $ imdbId x, toSql $ name x, toSql $ baconNumber x]) films)
    commit conn
    
storeActors :: [Actor] -> IO [Actor]
storeActors actors = do
    conn <- connectSqlite3 "bacon.db"
    putStrLn "Trying to store actors"
    newActors <- filterActors conn actors
    putStrLn "Filtered Actors"
    doStoreActors conn newActors
    putStrLn "Stored"
    return newActors
    
filterActors :: Connection -> [Actor] -> IO [Actor]
filterActors conn actors = do
    let actorIds = map toSql $ map imdbId actors
    let query = "SELECT imdbId FROM actor WHERE imdbId IN (" ++ parametrize actorIds ++ ")"
    res <- quickQuery' conn query actorIds
    
    let existingIds = map fromSql (map head res)
    
    return (filter (\x -> not(elem (toSql $ imdbId x) existingIds)) actors)
    
doStoreActors :: Connection -> [Actor] -> IO()
doStoreActors conn actors = do
    stmt <- prepare conn "INSERT INTO actor (imdbId, name, baconNumber) VALUES (?, ?, ?)"
    executeMany stmt (map (\x -> [toSql $ imdbId x, toSql $ name x, toSql $ baconNumber x]) actors)
    commit conn

parametrize :: [SqlValue] -> String
parametrize values = intercalate "," $ replicate (length values) "?"

    
{-storeURLs :: [URL] -> IO ()
storeURLs [] = return ()
storeURLs xs =
     do conn <- connectSqlite3 "urls.db"
        stmt <- prepare conn "INSERT INTO urls (url) VALUES (?)"
        executeMany stmt (map (\x -> [toSql x]) xs)
        commit conn        

printURLs :: IO ()
printURLs = do urls <- getURLs
               mapM_ print urls

getURLs :: IO [URL]
getURLs = do conn <- connectSqlite3 "urls.db"
             res <- quickQuery' conn "SELECT url FROM urls" []
             return $ map fromSql (map head res)
             
unfoldDB :: IO ()
unfoldDB = do urls <- getURLs
              process urls

process :: [URL] -> IO ()
process [] = return ()
process (x:xs) = do print $ "Processing : " ++ x
                    urlContent <- downloadURL x
                    storeURLs (parseURLs urlContent)
                    process xs
-}