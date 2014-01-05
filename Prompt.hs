-- Custom prompt functions - You may have seen this before in the first coursework assignment...
module Prompt(prompt, multilinePrompt) where


-- | Basic prompt - prints 'str ~>' to the console (for input str)
prompt :: String -> IO String
prompt str = do
    putStr $ str ++ " ~> "
    input <- getLine
    putStrLn ""
    return input


-- | Prefixes the prompt with one or more lines of instructions
multilinePrompt :: String -> [String] -> IO String
multilinePrompt promptName description = do
    putLines description
    prompt promptName

    
-- | Print out multiple lines to the console
putLines :: [String] -> IO()
putLines [] = return ()
putLines (line:lines) = do
    putStrLn line
    putLines lines