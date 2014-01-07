module Search(runSearch, searchResults) where

import Text.Regex

import Config
import DataModel
import Prompt
import SearchParser
import URLDownloader

-- | Replace spaces in the given string with + for URL friendliness
subSpace :: String -> String
subSpace str = subRegex rx str "+"
    where rx = mkRegex " "


-- | Builds an appropriate URL for the desired search query
searchUrl :: String -> String
searchUrl nm = imdbBaseUrl ++ "find?s=nm&exact=true&q=" ++ (subSpace nm)


-- | Runs a search from the command prompt
runSearch :: IO (Maybe Actor)
runSearch = do
    actorName <- prompt "Who do you want to start with?"
    actors <- searchResults actorName
    selectedActorIndex <- selectActor $ fst actors
    case selectedActorIndex of
        Just ix -> do
            let actor = (snd actors)!!ix
            return $ Just actor
        _ -> return Nothing
    

-- | Fetch and parse search results
searchResults :: String -> IO ([Actor], [Actor])
searchResults actorName = do
    searchResultsPage <- downloadURL $ searchUrl actorName
    return (parseSearchResults searchResultsPage, cleanedSearchResults searchResultsPage)


-- | Command-prompt handler for selecting one from several matching results, if appropriate.
selectActor :: [Actor] -> IO (Maybe Int)
selectActor [] = do
    putStrLn "No matching results"
    return Nothing
selectActor (actor:[]) = do
    putStrLn $ "One result found: " ++ (name actor)
    return (Just 0)
selectActor actors = do
    response <- multilinePrompt "" $ 
        ["We found " ++ (show $ length actors) ++ " matching entries:", ""] ++ 
        listNames actors ++ 
        ["", "Who do you want to seed your database with? " ++ (show [1..(length actors)]), "(press any other key to cancel)"]
        
    if ((length $ filter (== response) (map show [1..(length actors)])) > 0) then 
        return $ Just ((read response::Int) - 1)
    else do
        putStrLn "No changes made"
        return Nothing

