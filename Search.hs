module Search where

import Text.Regex

import DataModel
import Prompt
import SearchParser
import URLDownloader


subSpace :: String -> String
subSpace str = subRegex rx str "+"
    where rx = mkRegex " "

    
searchUrl :: String -> String
searchUrl nm = imdbBaseUrl ++ "find?s=nm&exact=true&q=" ++ (subSpace nm)


runSearch :: IO (Maybe Actor)
runSearch = do
    actorName <- prompt "Who do you want to start with?"
    if (length actorName > 0) then do
        searchResultsPage <- downloadURL $ searchUrl actorName
        let actors = parseSearchResults searchResultsPage
        selectActor actors
    else return Nothing
    
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
    
-- Build a numbered list of people
listNames :: [Actor] -> [String]
listNames = listNamesRec 0 -- Call through to the recursive function with index 0

listNamesRec :: Integer -> [Actor] -> [String]
listNamesRec _ [] = []
listNamesRec ix (actor:actors) = ((show (ix + 1)) ++ ": " ++ (name actor)):listNamesRec (ix + 1) actors
    

