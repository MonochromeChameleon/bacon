-- | Abstracts the actual database connection away from those classes where database access 
-- | is being triggered. This is the only file that should reference Database.HDBC.Sqlite3
-- | as that's a concrete implementation rather than the generic one that we ought to use
-- | everywhere else in the codebase.

module DatabaseConnector(withConnection, tryWithConnection) where

import Control.Concurrent
import Control.Exception
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO
import System.Random

import Config

dbName :: IO String
dbName = do
    curr <- currentDB
    case curr of
        Just db -> return db
        _ -> return "bacon.db"
    

-- |Wrapper for functions that require a connection to the database - any externally-exposed
-- |functions in this module that use a db connection should simply defer execution to a 
-- |private function with the connection as its last argument, to be executed by this wrapper 
-- |function. In turn, that means that we only have a single location in which the connection 
-- |is ever defined, and we can be confident that it will never be left open.
withConnection :: (Connection -> IO a) -> IO a
withConnection func = do
    db <- dbName
    conn <- connectSqlite3 db
    res <- func conn
    commit conn
    disconnect conn
    return res


-- | Write-only db connection handler that will handle a locked database and try again after
-- | a random delay. The delay is random so that the retry attempts don't become synchronized
-- | and cause a mutual lock.
tryWithConnection :: (Connection -> IO ()) -> IO ()
tryWithConnection func = do
    result <- try (withConnection func) :: IO (Either SqlError ())
    case result of
        Left (SqlError r t f) -> do
            putStrLn f
            tryAgain func
        Right r -> return ()


-- | Recursively re-attempts a failed DB operation after a random delay to avoid contention on
-- | multiple threads.
tryAgain :: (Connection -> IO ()) -> IO ()
tryAgain func = do
    putStrLn "Contention for DB Connection - waiting"
    randomDelay <- randomRIO(100000, 10000000) -- 0.1 to 10 seconds
    threadDelay randomDelay
    tryWithConnection func -- retry
