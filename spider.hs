module Spider where

import Network.HTTP
import Network.URI
import Data.Maybe

import BaconDB
import DataModel
import ActorParser
import FilmParser

imdbBaseUrl :: String
imdbBaseUrl = "http://www.imdb.com/"



catchUp :: BaconNumber -> Int -> IO()
catchUp baconNumber maxBacon | baconNumber == maxBacon = return()
                             | otherwise = do
    films <- loadFilmsWithBacon baconNumber
    newActors <- crawlFilms (baconNumber + 1) films
    
    crawl (baconNumber + 1) maxBacon


crawl :: BaconNumber -> Int -> IO()
crawl baconNumber maxBacon | baconNumber == maxBacon = return ()
                           | otherwise = do
    actors <- loadActorsWithBacon baconNumber
    films <- crawlActors baconNumber actors
    newActors <- crawlFilms (baconNumber + 1) films

    crawl (baconNumber + 1) maxBacon
    
    
crawlActors :: BaconNumber -> [Actor] -> IO [Film]
crawlActors = doCrawlActors []

doCrawlActors :: [Film] -> BaconNumber -> [Actor] -> IO [Film]
doCrawlActors films _ [] = return films
doCrawlActors films baconNumber (actor:actors) = do
    newFilms <- doCrawlActor baconNumber actor
    doCrawlActors (films ++ newFilms) baconNumber actors
    
doCrawlActor :: BaconNumber -> Actor -> IO [Film]
doCrawlActor baconNumber actor = do
    actorPage <- downloadURL $ actorUrl actor
    let films = getFilmographyDetails actorPage
    
    putStrLn $ "Found " ++ (show $ length films) ++ " films for " ++ (name actor) ++ " with baconNumber " ++ (show baconNumber)
    
    newFilms <- storeFilms actor films
    
    putStrLn $ "Stored " ++ (show $ length newFilms) ++ " new films"
    
    return newFilms

crawlFilms :: BaconNumber -> [Film] -> IO [Actor]
crawlFilms = doCrawlFilms []

doCrawlFilms :: [Actor] -> BaconNumber -> [Film] -> IO [Actor]
doCrawlFilms actors _ [] = return actors
doCrawlFilms actors baconNumber (film:films) = do
    newActors <- doCrawlFilm baconNumber film
    doCrawlFilms (actors ++ newActors) baconNumber films

doCrawlFilm :: BaconNumber -> Film -> IO [Actor]
doCrawlFilm baconNumber film = do
    filmPage <- downloadURL $ filmUrl film
    let isAdult = checkAdultStatus filmPage
    
    if isAdult then do
        putStrLn "THIS IS RUDE!±!!!"
        deleteFilm film
        return []
    else do
        castPage <- downloadURL $ fullCastUrl film
        let actors = getCastDetails baconNumber castPage
    
        storeFilmActors baconNumber film actors
    
    
storeFilmActors :: BaconNumber -> Film -> [Actor] -> IO [Actor]
storeFilmActors baconNumber film actors = do
    putStrLn $ "Found " ++ (show $ length actors) ++ " actors for " ++ (title film) ++ " with baconNumber " ++ (show baconNumber)
    newActors <- storeActors film actors
    putStrLn $ "Stored " ++ (show $ length newActors) ++ " new actors"
    return newActors

    
actorUrl :: Actor -> URL
actorUrl actor = imdbBaseUrl ++ "name/" ++ (actor_id actor) ++ "/"

filmUrl :: Film -> URL
filmUrl film = imdbBaseUrl ++ "title/" ++ (film_id film) ++ "/"

fullCastUrl :: Film -> URL
fullCastUrl film = imdbBaseUrl ++ "title/" ++ (film_id film) ++ "/fullcredits"
    
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
          
          
          
          
          
{-crawlActor :: BaconNumber -> Int -> Actor -> IO()
crawlActor baconNumber maxBacon actor = do
    actorPage <- downloadURL $ actorUrl actor
    let films = getFilmographyDetails actorPage
    
    putStrLn $ "Found " ++ (show $ length films) ++ " films for " ++ (name actor) ++ " with baconNumber " ++ (show baconNumber)
    
    newFilms <- storeFilms actor $ convertDetails baconNumber films
    
    putStrLn $ "Stored " ++ (show $ length newFilms) ++ " new films"
    
    newActors <- crawlFilms (baconNumber + 1) maxBacon newFilms

    if (baconNumber < maxBacon) then do
        crawlActors (baconNumber + 1) maxBacon newActors
    else
        return () -}