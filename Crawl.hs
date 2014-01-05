module Crawl(crawlEntities) where

import ActorParser
import BaconDB
import Config
import DataModel
import FilmParser
import ORM
import URLDownloader
import Utils


-- | Recursively crawl all the supplied entities, storing any new actors found in the casts
-- | of those films.
crawlEntities :: Crawl a => Bacon -> [a] -> IO ()
crawlEntities _ [] = return ()
crawlEntities bacon (e:es) = do
    doCrawl bacon e
    crawlEntities bacon es
    
    
-- | Return the actor page URL for the given actor
actorUrl :: Actor -> String
actorUrl actor = imdbBaseUrl ++ "name/" ++ (imdbid actor) ++ "/"


-- | Return the film summary page URL for the given film
filmUrl :: Film -> String
filmUrl film = imdbBaseUrl ++ "title/" ++ (imdbid film) ++ "/"


-- | Return the film full cast page URL for the given film
fullCastUrl :: Film -> String
fullCastUrl film = imdbBaseUrl ++ "title/" ++ (imdbid film) ++ "/fullcredits"


-- | Typeclass for our crawlable entities (which, admittedly, are all our entities)
class (Entity a) => Crawl a where
    doCrawl :: Int -> a -> IO()


instance Crawl Film where
    -- | Crawl an individual film page, first checking for adult films and removing 
    -- | them, and storing the cast list for any other films
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
            putStrLn $ "Found " ++ (pluralize (length actors) "actor") ++ " for " ++ (title film) ++ " with " ++ (pluralize bacon "slice") ++ " of bacon"
            storeProcessingResult film actors
            

instance Crawl Actor where
    -- | Crawl an individual actor page, storing their filmographies
    doCrawl bacon actor = do
        actorPage <- downloadURL $ actorUrl actor
        let films = parseActor bacon actorPage
        putStrLn $ "Found " ++ (pluralize (length films) "film") ++ " for " ++ (name actor)
        storeProcessingResult actor films

