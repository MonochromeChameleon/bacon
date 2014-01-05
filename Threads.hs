module Threads (multithread) where

import Control.Concurrent
import GHC.Conc (numCapabilities)


-- ====================================== --
-- == GENERIC MULTITHREADED PROCESSING == -- 
-- ====================================== --
        
-- |Takes a reference to a function and executes said function on all available cores. The function should take three arguments:
-- | number of cores, index of the core currently executing, and an MVar Boolean flag so that we can detect when processing is complete
multithread :: (Int -> Int -> MVar Bool -> IO()) -> IO()
multithread toExecute = do
    
    -- Determine the number of available cores for processing
    let cores = numCapabilities
    
    putStrLn $ "Processing with " ++ (show cores) ++ " cores"
    
    -- Instigate all available threads, and retrieve a list containing their associated completion flags.
    mvars <- doMultiThread toExecute cores 0

    putStrLn "All threads executing"
    
    -- Block the main thread until processing is complete on all subsidiary threads
    readMVars mvars
    putStrLn "DONE"
    
    
-- | Recursively kick off all threads, and return an array of their MVar completion flags
doMultiThread :: (Int -> Int -> MVar Bool -> IO()) -> Int -> Int -> IO [MVar Bool]
doMultiThread toExecute cores ix | cores <= ix = return []
                                 | otherwise = do
    mvar <- createThread (toExecute cores) ix
    mvars <- doMultiThread toExecute cores (ix + 1)
    
    return (mvar:mvars)


-- | Creates an individual thread, returning its Boolean completion flag.
createThread :: (Int -> MVar Bool -> IO()) -> Int ->  IO (MVar Bool)
createThread toExecute ix = do
    putStrLn $ "Initializing thread " ++ (show ix)

    sync <- newEmptyMVar :: IO (MVar Bool)
    forkIO (toExecute ix sync)
    return sync


-- | Reads an array of Boolean MVar flags, disregarding the values.
readMVars :: [MVar Bool] -> IO()
readMVars [] = return ()
readMVars (mvar:mvars) = do
    synced <- takeMVar mvar
    readMVars mvars

