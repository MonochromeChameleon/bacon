module Config where

import DataModel
import System.IO
import ORM

switchConfig :: IO ()
switchConfig = return () --QQ

configFile :: String
configFile = "bacon.conf"


imdbBaseUrl :: String
imdbBaseUrl = "http://www.imdb.com/"


data ConfigState = ConfigState { current :: Maybe Instance,
                                 options :: [Instance] } deriving (Eq, Show, Read)

data Instance = Instance { dbname :: String,
                           seed_ :: Actor } deriving (Eq, Show, Read)
                           

currentDB :: IO (Maybe String)
currentDB = do
    conf <- getConfig 
    let curr = current conf
    case (curr) of
        Just c -> return $ Just (dbname c)
        _ -> return Nothing


reset :: IO ()
reset = do
    let defaultConf = ConfigState { current = Nothing, options = [] }
    writeConfig defaultConf

getFileHandle :: IO Handle
getFileHandle = openFile configFile ReadWriteMode

writeConfig :: ConfigState -> IO ()
writeConfig conf = writeFile configFile $ show conf

getConfig :: IO ConfigState
getConfig = do
    txt <- readFile configFile
    let inst = read txt::ConfigState
    length txt `seq` (return inst)

addConfig :: Actor -> IO()
addConfig actor = do
    currentConf <- getConfig
    let newInstance = Instance { dbname = (imdbid actor) ++ ".db", seed_ = actor }
    let newConf = doAddConfig currentConf newInstance
    
    writeFile configFile $ show newConf

doAddConfig :: ConfigState -> Instance -> ConfigState
doAddConfig currentConf newInstance = ConfigState { current = Just newInstance, options = (newInstance:(options currentConf)) }
