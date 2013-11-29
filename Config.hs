module Config where

import DataModel
import System.IO

configFile :: String
configFile = "bacon.conf"

data ConfigState = ConfigState { current :: Maybe Instance,
                                 options :: [Instance] } deriving (Eq, Show, Read)

data Instance = Instance { dbname :: String,
                           seed_ :: Actor } deriving (Eq, Show, Read)


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
    handle <- getFileHandle
    txt <- hGetContents handle
    putStrLn txt
    let inst = read txt::ConfigState
    hClose handle

    return inst

addConfig :: String -> Actor -> IO()
addConfig dbName actor = do
    currentConf <- getConfig
    
    handle <- getFileHandle
    let newConf = doAddConfig currentConf Instance { dbname = dbName, seed_ = actor }
    hPutStr handle (show newConf)
    hClose handle

doAddConfig :: ConfigState -> Instance -> ConfigState
doAddConfig currentConf newInstance = ConfigState { current = Just newInstance, options = (newInstance:(options currentConf)) }
