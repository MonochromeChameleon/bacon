module Spider (crawl) where

import Control.Concurrent

import BaconDB
import Crawl
import DataModel
import ORM
import Threads
import Utils


-- | Start a new crawl operation - we can always start at 0 as it will iteratively go up the
-- | bacon numbers until we reach an unprocessed level, or our maximum bacon value.
crawl :: Int -> IO()
crawl = loadAndCrawlActors 0

        
-- =================================== --
-- == ACTOR PAGE CRAWLING & PARSING == --
-- =================================== --

-- | Here we look up all unprocessed actors of the appropriate bacon level, fetch their filmographies
-- | and then call through to crawl the films
loadAndCrawlActors :: Bacon -> Int -> IO()
loadAndCrawlActors bacon maxBacon | bacon >= maxBacon = return ()
                                  | otherwise = do
    putStrLn $ "Loading actors with " ++ (pluralize bacon "slice") ++ " of bacon"
    actors <- loadEntitiesWithBacon ActorType bacon
    
    if length actors == 0 then do
        putStrLn "Finished loading actors"
        loadAndCrawlFilms bacon maxBacon
    else do
        putStrLn $ "Processing " ++ (show $ length actors) ++ " actors"
        multithread $ threadedCrawlActors bacon actors
        loadAndCrawlActors bacon maxBacon
        

-- | Multithread-enabled actor crawler (can also be run on a single thread)
threadedCrawlActors :: Int -> [Actor] -> Int -> Int -> MVar Bool -> IO()
threadedCrawlActors bacon actors cores ix sync = do
    let numToProcess = (div (length actors) cores) + 1
    let dropped = drop (numToProcess * ix) actors
    let actorsToProcess = take numToProcess dropped
    
    crawlEntities bacon actorsToProcess
    
    putMVar sync True     


-- ================================== --
-- == FILM PAGE CRAWLING & PARSING == --
-- ================================== --

-- | Here we look up all unprocessed films, ordered by release date (ASC) and process their
-- | cast lists, before calling through to crawl the newly-found actors
loadAndCrawlFilms :: Bacon -> Int -> IO()
loadAndCrawlFilms bacon maxBacon | bacon >= maxBacon = return ()
                                 | otherwise = do
    putStrLn $ "Loading films with " ++ (pluralize bacon "slice") ++ " of bacon"
    films <- loadEntitiesWithBacon FilmType bacon
    if length films == 0 then do
        putStrLn "Finished loading films"
        loadAndCrawlActors (bacon + 1) maxBacon
    else do
        putStrLn $ "Processing " ++ (show $ length films) ++ " films"
        multithread $ threadedCrawlFilms (bacon + 1) films
        loadAndCrawlFilms bacon maxBacon
        

-- | Multithread-enabled film crawler (can also be run on a single thread)        
threadedCrawlFilms :: Int -> [Film] -> Int -> Int -> MVar Bool -> IO()
threadedCrawlFilms bacon films cores ix sync = do
    let numToProcess = (div (length films) cores) + 1
    let dropped = drop (numToProcess * ix) films
    let filmsToProcess = take numToProcess dropped
    
    crawlEntities bacon filmsToProcess
    
    putMVar sync True
        
