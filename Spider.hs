module Spider where

import Network.HTTP
import Network.URI
import Data.Maybe

import BaconDB
import DataModel
import ActorParser
import FilmParser
import StringUtils


-- Decides what stage we had reached previously, and calls our next crawl operation
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

-- Here we look up all unprocessed actors of the appropriate bacon level, fetch their filmographies
-- and then call through to crawl the films
loadAndCrawlActors :: Bacon -> Int -> IO()
loadAndCrawlActors bacon maxBacon | bacon == maxBacon = return ()
                                  | otherwise = do
    actors <- loadActorsWithBacon bacon
    
    if length actors == 0 then
        loadAndCrawlActors (bacon + 1) maxBacon
    else do
        putStrLn $ "Processing " ++ (show $ length actors) ++ " actors"
        crawlActors actors
        loadAndCrawlFilms (bacon + 1) maxBacon
    
    
-- Recursively crawl all the supplied actors
crawlActors :: [Actor] -> IO ()
crawlActors [] = return ()
crawlActors (actor:actors) = do
    doCrawlActor actor
    crawlActors actors

-- Crawl an individual actor page, storing any new films in their filmography
doCrawlActor :: Actor -> IO ()
doCrawlActor actor = do
    actorPage <- downloadURL $ actorUrl actor
    let films = getFilmographyDetails actorPage
    putStrLn $ "Found " ++ (show $ length films) ++ " films for " ++ (name actor)
    storeFilms actor films

-- ================================== --
-- == FILM PAGE CRAWLING & PARSING == --
-- ================================== --

-- Here we look up all unprocessed films, ordered by release date (ASC) and process their
-- cast lists, before calling through to crawl the newly-found actors
loadAndCrawlFilms :: Bacon -> Int -> IO()
loadAndCrawlFilms bacon maxBacon = do
    films <- loadUnprocessedFilms
    
    putStrLn $ "Processing " ++ (show $ length films) ++ " films"
    
    crawlFilms bacon films
    
    loadAndCrawlActors bacon maxBacon 


-- Recursively crawl all the supplied films, storing any new actors found in the casts
-- of those films.
crawlFilms :: Bacon -> [Film] -> IO ()
crawlFilms _ [] = return ()
crawlFilms bacon (film:films) = do
    doCrawlFilm bacon film
    crawlFilms bacon films


-- Crawl an individual film page, first checking for adult films and removing 
-- them, and returning the cast list for any other films
doCrawlFilm :: Bacon -> Film -> IO ()
doCrawlFilm bacon film = do
    filmPage <- downloadURL $ filmUrl film
    let isAdult = checkAdultStatus filmPage
    
    if isAdult then do
        putStrLn "THIS IS RUDE!!!!"
        deleteFilm film
        return ()
    else do
        castPage <- downloadURL $ fullCastUrl film
        let actors = getCastDetails bacon castPage
        
        putStrLn $ "Found " ++ (show $ length actors) ++ " actors for " ++ (title film) ++ " with " ++ (pluralize bacon "slice") ++ " of bacon"
    
        storeActors film actors


-- ============================ --
-- == PAGE DOWNLOAD FUNCTION == --
-- ============================ --

downloadURL :: String -> IO String
downloadURL url = do
    resp <- simpleHTTP request
    case resp of
        Left x  -> return $ "Error connecting: " ++ show x
        Right r -> case rspCode r of
            (2,_,_) -> return $ rspBody r
            _       -> return $ show r

    where request = Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = ""}
          uri = fromJust $ parseURI url
