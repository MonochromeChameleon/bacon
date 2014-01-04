module Spider (crawl) where

import Control.Concurrent

import BaconDB
import DataModel
import ActorParser
import FilmParser
import Threads
import Parser
import URLDownloader
import ORM

-- | Decides what stage we had reached previously, and calls our next crawl operation
crawl :: Int -> IO()
crawl maxBacon = do

    processingStatus <- getProcessingStatus
    
    if (snd processingStatus) then
        loadAndCrawlActors (fst processingStatus) maxBacon
    else
        loadAndCrawlFilms (fst processingStatus) maxBacon
        
        
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
    
-- Recursively crawl all the supplied entities, storing any new actors found in the casts
-- of those films.
crawlEntities :: Crawl a => Bacon -> [a] -> IO ()
crawlEntities _ [] = return ()
crawlEntities bacon (e:es) = do
    doCrawl bacon e
    crawlEntities bacon es


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
        

        
pluralize :: Int -> String -> String
pluralize count str = (show count) ++ " " ++ (if count == 1 then str else str ++ "s")

instance Crawl Film where
    -- Crawl an individual film page, first checking for adult films and removing 
    -- them, and returning the cast list for any other films
    doCrawl bacon film = do
        filmPage <- downloadURL $ filmUrl film
        let isAdult = checkAdultStatus filmPage
        
        if isAdult then do
            putStrLn "Deleting adult film"
            delete film
            putStrLn "Deleted"
            return ()
        else do
            castPage <- downloadURL $ fullCastUrl film
            let actors = parseFilm bacon castPage
            putStrLn $ "Found " ++ (show $ length actors) ++ " actors for " ++ (title film) ++ " with " ++ (pluralize bacon "slice") ++ " of bacon"
            storeProcessingResult film actors
            
instance Crawl Actor where
    -- Crawl an individual actor page, returning their filmographies
    doCrawl bacon actor = do
        actorPage <- downloadURL $ actorUrl actor
        let films = parseActor bacon actorPage
        putStrLn $ "Found " ++ (show $ length films) ++ " films for " ++ (name actor)
        storeProcessingResult actor films
