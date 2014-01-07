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
    selectActor actors
    

-- | Fetch and parse search results
searchResults :: String -> IO [Actor]
searchResults actorName = do
    searchResultsPage <- downloadURL $ searchUrl actorName
    return $ parseSearchResults searchResultsPage


-- | Command-prompt handler for selecting one from several matching results, if appropriate.
selectActor :: [Actor] -> IO (Maybe Actor)
selectActor [] = do
    putStrLn "No matching results"
    return Nothing
selectActor (actor:[]) = do
    putStrLn $ "One result found: " ++ (name actor)
    return (Just actor)
selectActor actors = do
    response <- multilinePrompt "" $ 
        ["We found " ++ (show $ length actors) ++ " matching entries:", ""] ++ 
        listNames actors ++ 
        ["", "Who do you want to seed your database with? " ++ (show [1..(length actors)]), "(press any other key to cancel)"]
        
    if ((length $ filter (== response) (map show [1..(length actors)])) > 0) then 
        return $ Just (actors!!((read response::Int) - 1))
    else do
        putStrLn "No changes made"
        return Nothing


-- Build a numbered list of people - calls through to listNamesRec
listNames :: [Actor] -> [String]
listNames = listNamesRec 0 -- Call through to the recursive function with index 0

-- Recursive implementation of the listNames function
listNamesRec :: Integer -> [Actor] -> [String]
listNamesRec _ [] = []
listNamesRec ix (actor:actors) = ((show (ix + 1)) ++ ": " ++ (name actor)):listNamesRec (ix + 1) actors
    
