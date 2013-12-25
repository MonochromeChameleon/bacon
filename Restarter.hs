import System.ShQQ
import System.Cmd
import Data.List
import Data.Time

main = do
    killProcess
    startProcess

killProcess :: IO ()
killProcess = do
    putStrLn "Killing process"
    processId <- getProcessId ["Bacon", "crawl"]
    system $ "kill " ++ processId
    return ()
    
startProcess :: IO ()
startProcess = do
    putStrLn "And off we go again..."
    system "./Bacon crawl 6 +RTS -N4"
    return ()
    
getProcessId :: [String] -> IO String
getProcessId greps = do
    let cmd = buildCommand "ps ax" greps
    stout <- readShell cmd
    let processDetails = filter (\x -> not (isInfixOf "grep" x)) $ lines stout
    if length processDetails > 0 then
        return $ fst (span (\x -> x /= ' ') (trim $ head processDetails))
    else
        return ""
    
    
buildCommand :: String -> [String] -> String 
buildCommand prefix [] = prefix
buildCommand prefix (grep:greps) = buildCommand (prefix ++ "|grep " ++ grep) greps 