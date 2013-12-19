module Spider where

import Language.Haskell.TH.Ppr -- cabal install template-haskell
import Network.HTTP.Conduit -- cabal install http-conduit
import Control.Concurrent
import Data.Word
import Data.Either
import Control.Exception
import qualified Data.ByteString.Lazy as L
import GHC.Conc (numCapabilities)

import BaconDB
import DataModel
import ActorParser
import FilmParser
import StringUtils
import Threads

-- Decides what stage we had reached previously, and calls our next crawl operation
crawl :: Int -> IO()
crawl maxBacon = do

    putStrLn $ "Processing with " ++ (show numCapabilities) ++ " cores"
    
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
loadAndCrawlActors bacon maxBacon | bacon >= maxBacon = return ()
                                  | otherwise = do
    actors <- loadActorsWithBacon bacon
    
    if length actors == 0 then do
        putStrLn "Finished loading actors"
        loadAndCrawlFilms bacon maxBacon
    else do
        putStrLn $ "Processing " ++ (show $ length actors) ++ " actors"
        multithread $ threadedCrawlActors bacon actors
        loadAndCrawlActors bacon maxBacon
        
threadedCrawlActors :: Int -> [Actor] -> Int -> Int -> MVar Bool -> IO()
threadedCrawlActors bacon actors cores ix sync = do
    putStrLn $ "Crawling on core " ++ (show $ ix + 1) ++ " of " ++ (show cores)
    let numToProcess = (div (length actors) cores) + 1
    let dropped = drop (numToProcess * ix) actors
    let actorsToProcess = take numToProcess dropped
    
    crawlActors bacon actorsToProcess
    
    putMVar sync True 
    
    
-- Recursively crawl all the supplied actors
crawlActors :: Bacon -> [Actor] -> IO ()
crawlActors _ [] = return ()
crawlActors bacon (actor:actors) = do
    doCrawlActor bacon actor
    crawlActors bacon actors

-- Crawl an individual actor page, storing any new films in their filmography
doCrawlActor :: Bacon -> Actor -> IO ()
doCrawlActor bacon actor = do
    actorPage <- downloadURL $ actorUrl actor
    let films = getFilmographyDetails bacon actorPage
    putStrLn $ "Found " ++ (show $ length films) ++ " films for " ++ (name actor)
    storeFilms actor films

-- ================================== --
-- == FILM PAGE CRAWLING & PARSING == --
-- ================================== --

-- Here we look up all unprocessed films, ordered by release date (ASC) and process their
-- cast lists, before calling through to crawl the newly-found actors
loadAndCrawlFilms :: Bacon -> Int -> IO()
loadAndCrawlFilms bacon maxBacon | bacon >= maxBacon = return ()
                                 | otherwise = do
    films <- loadFilmsWithBacon bacon
    if length films == 0 then do
        putStrLn "Finished loading films"
        loadAndCrawlActors (bacon + 1) maxBacon
    else do
        putStrLn $ "Processing " ++ (show $ length films) ++ " films"
        multithread $ threadedCrawlFilms (bacon + 1) films
        loadAndCrawlFilms bacon maxBacon
        
        
threadedCrawlFilms :: Int -> [Film] -> Int -> Int -> MVar Bool -> IO()
threadedCrawlFilms bacon films cores ix sync = do
    putStrLn $ "Crawling on core " ++ (show $ ix + 1) ++ " of " ++ (show cores)
    let numToProcess = (div (length films) cores) + 1
    let dropped = drop (numToProcess * ix) films
    let filmsToProcess = take numToProcess dropped
    
    crawlFilms bacon filmsToProcess
    
    putMVar sync True


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
downloadURL = tryDownloadURL 0


tryDownloadURL :: Integer -> String -> IO String
tryDownloadURL 4 _ = return ""
tryDownloadURL attempts url = do
    result <- try (simpleHttp url) :: IO (Either HttpException L.ByteString)
    let e = lefts [result]
    if length e == 0 then do
        let text_bs = head.rights $ [result]
        let html_word8 = L.unpack text_bs :: [Word8]
        return $ bytesToString html_word8 :: IO String
    else do
        putStrLn $ "Download Error: " ++ url
        -- Recurse until we get a result. Is this dangerous?
        tryDownloadURL (attempts + 1) url

