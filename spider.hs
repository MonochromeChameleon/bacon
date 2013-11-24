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

crawlActors :: BaconNumber -> Int -> [URL] -> IO()
crawlActors _ _ [] = return ()
crawlActors baconNumber maxBacon (url:urls) = do
    crawlActor baconNumber maxBacon url
    crawlActors baconNumber maxBacon urls
    return ()

crawlActor :: BaconNumber -> Int -> URL -> IO ()
crawlActor baconNumber maxBacon url = do
    actorPage <- downloadURL url
    let films = getFilmographyDetails actorPage
    
    putStrLn ("got film details: " ++ url ++ ", baconNumber: " ++ (show baconNumber))
    
    newFilms <- storeFilms $ convertDetails baconNumber films

    putStrLn "stored new films"

    let urls = map filmUrl newFilms
    crawlFilms (baconNumber + 1) maxBacon urls

crawlFilms :: BaconNumber -> Int -> [URL] -> IO()
crawlFilms _ _ [] = return ()
crawlFilms baconNumber maxBacon (url:urls) = do
    crawlFilm baconNumber maxBacon url
    crawlFilms baconNumber maxBacon urls
    return ()
    
crawlFilm :: BaconNumber -> Int -> URL -> IO()
crawlFilm baconNumber maxBacon url = do
    filmPage <- downloadURL url
    let actors = getCastDetails filmPage
    newActors <- storeActors $ convertDetails baconNumber actors
    let urls = map actorUrl newActors
    if (baconNumber < maxBacon) then do
        crawlActors baconNumber maxBacon urls
    else
        return ()
    
actorUrl :: Actor -> URL
actorUrl actor = imdbBaseUrl ++ "name/" ++ (imdbId actor) ++ "/"

filmUrl :: Film -> URL
filmUrl film = imdbBaseUrl ++ "title/" ++ (imdbId film) ++ "/fullcredits"
    
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