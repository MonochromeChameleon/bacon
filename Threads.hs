module Threads where

import Control.Concurrent
import GHC.Conc (numCapabilities)


-- ====================================== --
-- == GENERIC MULTITHREADED PROCESSING == -- 
-- ====================================== --
        
-- |Takes a reference to a function and executes said function on all available cores. The function should take three arguments:
-- | number of cores, index of the core currently executing, and an MVar Boolean flag so that we can detect when processing is complete
multithread :: (Int -> Int -> MVar Bool -> IO()) -> IO()
multithread toExecute = do
    let cores = numCapabilities
    
    lock <- newEmptyMVar :: IO (MVar Bool)
    
    putStrLn "Splitting threads"
    mvars <- doMultiThread toExecute cores 0
    putStrLn "All Splat"
    
    readMVars mvars
    putStrLn "DONE"
    
    
doMultiThread :: (Int -> Int -> MVar Bool -> IO()) -> Int -> Int -> IO [MVar Bool]
doMultiThread toExecute cores ix | cores <= ix = return []
                                 | otherwise = do
    mvar <- createThread (toExecute cores) ix
    mvars <- doMultiThread toExecute cores (ix + 1)
    
    return (mvar:mvars)

    
createThread :: (Int -> MVar Bool -> IO()) -> Int ->  IO (MVar Bool)
createThread toExecute ix = do
    sync <- newEmptyMVar :: IO (MVar Bool)
    forkIO (toExecute ix sync)
    return sync


readMVars :: [MVar Bool] -> IO()
readMVars [] = return ()
readMVars (mvar:mvars) = do
    synced <- takeMVar mvar
    readMVars mvars


