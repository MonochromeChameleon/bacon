-- | A handy little script for killing and restarting the bacon crawl process. I configured this
-- | to run once a day on a cron job while I was on holiday, just in case the crawling process
-- | hung at any point.
import System.ShQQ
import System.Cmd
import Data.List
import Data.Time

-- | Kills and starts the crawl
main :: IO ()
main = do
    killProcess
    startProcess


-- | Identify the crawl process and kill it
killProcess :: IO ()
killProcess = do
    putStrLn "Killing process"
    processId <- getProcessId ["Bacon", "crawl"]
    system $ "kill " ++ processId
    return ()
    

-- | Start a new crawl again
startProcess :: IO ()
startProcess = do
    putStrLn "And off we go again..."
    system "./Bacon crawl 6 +RTS -N4"
    return ()
    

-- | Identifies the process id corresponding to the provided keywords
getProcessId :: [String] -> IO String
getProcessId greps = do
    let cmd = buildCommand "ps ax" greps
    stout <- readShell cmd
    let processDetails = filter (\x -> not (isInfixOf "grep" x)) $ lines stout
    if length processDetails > 0 then
        return $ fst (span (\x -> x /= ' ') (trim $ head processDetails))
    else
        return ""
    

-- | Recursively builds up a process identifying console command with the desired grep filters    
buildCommand :: String -> [String] -> String 
buildCommand prefix [] = prefix
buildCommand prefix (grep:greps) = buildCommand (prefix ++ "|grep " ++ grep) greps 