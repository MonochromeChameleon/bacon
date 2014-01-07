module Config(imdbBaseUrl, switchConfig, addConfig, currentDB) where

import System.IO

import DataModel
import ORM

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
    doSwitchConfig availableOptions


-- | Add a new base configuration to the config file and set that base as the current config.
addConfig :: Actor -> IO()
addConfig actor = do
    currentConf <- getConfig
    let newInstance = Instance { dbname = (imdbid actor) ++ ".db", seed_ = actor }
    let newConf = doAddConfig currentConf newInstance
    writeConfig newConf
    

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


doSwitchConfig :: [Instance] -> IO ()
doSwitchConfig [] = putStrLn "No instances are currently configured - please add a new config base first"
doSwitchConfig (inst:[]) = putStrLn "Only one instance is currently configured - nothing to switch. You can add new config though."
doSwitchConfig instances = do
    return () --QQ


-- | Creates a new config state with the new instance as the current option
doAddConfig :: ConfigState -> Instance -> ConfigState
doAddConfig currentConf newInstance = ConfigState { current = Just newInstance, options = (newInstance:(options currentConf)) }


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

