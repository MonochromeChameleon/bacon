module Config(imdbBaseUrl, switchConfig, addConfig, currentDB) where

import System.IO

import DataModel
import ORM
import Prompt

-- | Constant name for the config file§
configFile :: String
configFile = "bacon.conf"


-- | Constant imdb URL. I feel like this should be somewhere else, but putting it in "Config" does seem reasonable.
imdbBaseUrl :: String
imdbBaseUrl = "http://www.imdb.com/"


-- | Display the available configured databases and allow the user to choose a current one for crawling and querying
switchConfig :: IO ()
switchConfig = do
    conf <- getConfig
    let availableOptions = options conf
    doSwitchConfig conf availableOptions


-- | Add a new base configuration to the config file and set that base as the current config, first checking for duplicates.
addConfig :: Actor -> IO Bool
addConfig actor = do
    currentConf <- getConfig
    let inst = getConfigInstance currentConf $ imdbid actor 
    case inst of
        Just i -> do
            putStrLn "Actor already exists in config - setting as current"
            writeConfig $ doSetConfig currentConf i
            return False
        _ -> do
            let newInstance = Instance { dbname = (imdbid actor) ++ ".db", seed_ = actor }
            let newConf = doAddConfig currentConf newInstance
            writeConfig newConf
            putStrLn "New config added"
            return True
    

-- | Returns the name of the currently-configured database file
currentDB :: IO (Maybe String)
currentDB = do
    conf <- getConfig 
    let curr = current conf
    case (curr) of
        Just c -> return $ Just (dbname c)
        _ -> return Nothing

    
-- Private methods

-- | Record for the overall config state of the application - a current config and all the available options
data ConfigState = ConfigState { current :: Maybe Instance,
                                 options :: [Instance] } deriving (Eq, Show, Read)


-- | Record for a configuration instance
data Instance = Instance { dbname :: String,
                           seed_ :: Actor } deriving (Eq, Show, Read)
                           

-- | Looks up a config instance by the associated imdb id
getConfigInstance :: ConfigState -> ImdbID -> Maybe Instance
getConfigInstance conf imdbId = if isConfigured then Just $ instances!!0 else Nothing
    where isConfigured = (length $ instances) > 0
          instances = filter (\c -> (imdbid $ seed_ c) == imdbId) $ options conf
                           

-- | Switches the current database to the desired instance
doSwitchConfig :: ConfigState -> [Instance] -> IO ()
doSwitchConfig _ [] = putStrLn "No instances are currently configured - please add a new config base first"
doSwitchConfig _ (inst:[]) = putStrLn "Only one instance is currently configured - nothing to switch. You can add new config though."
doSwitchConfig conf instances = do
    response <- multilinePrompt "" $ 
        ["You have " ++ (show $ length instances) ++ " configurations available:", ""] ++ 
        (listNames $ map seed_ instances) ++ 
        ["", "Who do you want to switch to? " ++ (show [1..(length instances)]), "(press any other key to cancel)"]
        
    if ((length $ filter (== response) (map show [1..(length instances)])) > 0) then do
        let newConf = doSetConfig conf (instances!!((read response::Int) - 1))
        writeConfig newConf
        putStrLn "Updated"
    else do
        putStrLn "No changes made"


-- | Creates a new config state with the new instance as the current option, and added to the existing available options
doAddConfig :: ConfigState -> Instance -> ConfigState
doAddConfig currentConf newInstance = ConfigState { current = Just newInstance, options = (newInstance:(options currentConf)) }


-- | Creates a new config state with the desired instance as the current option, leaving options unchanged
doSetConfig :: ConfigState -> Instance -> ConfigState
doSetConfig currentConf selectedInstance = ConfigState { current = Just selectedInstance, options = (options currentConf) }


-- | Writes a config state back to the conf file
writeConfig :: ConfigState -> IO ()
writeConfig conf = writeFile configFile $ show conf


-- | Retrieve the config state from the conf file
getConfig :: IO ConfigState
getConfig = do
    txt <- readFile configFile
    let inst = read txt::ConfigState
    length txt `seq` (return inst) -- We have to greedily evaluate the contents of the file to avoid file contention


-- | Utility function for initializing a new config file. Deliberately not exposed as public, so that
-- | you can't accidentally delete your config.
reset :: IO ()
reset = do
    let defaultConf = ConfigState { current = Nothing, options = [] }
    writeConfig defaultConf

