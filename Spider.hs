module Spider where

import Network.HTTP
import Network.URI
import Data.Maybe

import BaconDB
import DataModel
import ActorParser
import FilmParser
import StringUtils


-- Let's have a nice alias
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
    
    
-- alias for the recursive call
crawlActors :: [Actor] -> IO [Film]
crawlActors = doCrawlActors []


-- Recursively crawl all the supplied actors, returning the aggregated filmographies
-- of those actors.
doCrawlActors :: [Film] -> [Actor] -> IO [Film]
doCrawlActors films [] = return films
doCrawlActors films (actor:actors) = do
    newFilms <- doCrawlActor actor
    doCrawlActors (films ++ newFilms) actors


-- Crawl an individual actor page, returning their filmography
doCrawlActor :: Actor -> IO [Film]
doCrawlActor actor = do
    actorPage <- downloadURL $ actorUrl actor
    let films = getFilmographyDetails actorPage
    
    putStrLn $ "Found " ++ (show $ length films) ++ " films for " ++ (name actor)
    
    newFilms <- storeFilms actor films
    
    putStrLn $ "Stored " ++ (show $ length newFilms) ++ " new films"
    
    return newFilms


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


-- Alias for the recursive call    
crawlFilms :: Bacon -> [Film] -> IO [Actor]
crawlFilms = doCrawlFilms []


-- Recursively crawl all the supplied films, returning the aggregated casts
-- of those films.
doCrawlFilms :: [Actor] -> Bacon -> [Film] -> IO [Actor]
doCrawlFilms actors _ [] = return actors
doCrawlFilms actors bacon (film:films) = do
    newActors <- doCrawlFilm bacon film
    doCrawlFilms (actors ++ newActors) bacon films


-- Crawl an individual film page, first checking for adult films and removing 
-- them, and returning the cast list for any other films
doCrawlFilm :: Bacon -> Film -> IO [Actor]
doCrawlFilm bacon film = do
    filmPage <- downloadURL $ filmUrl film
    let isAdult = checkAdultStatus filmPage
    
    if isAdult then do
        putStrLn "THIS IS RUDE!!!!"
        deleteFilm film
        return []
    else do
        castPage <- downloadURL $ fullCastUrl film
        let actors = getCastDetails bacon castPage
    
        storeFilmActors bacon film actors
    

-- Store the film-actor relationship so that we can track back to our original actor later on.
storeFilmActors :: Bacon -> Film -> [Actor] -> IO [Actor]
storeFilmActors bacon film actors = do
    putStrLn $ "Found " ++ (show $ length actors) ++ " actors for " ++ (title film) ++ " with " ++ (pluralize bacon "slice") ++ " of bacon"
    newActors <- storeActors film actors
    putStrLn $ "Stored " ++ (show $ length newActors) ++ " new actors"
    return newActors


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
