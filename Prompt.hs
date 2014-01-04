-- Custom prompt functions
module Prompt where


-- IO: Basic prompt - prints 'Str ~>' to the console (for input str)
inlinePrompt :: String -> IO String
inlinePrompt str = do
    putStr $ str ++ " ~> "
    input <- getLine
    return input


-- IO: Same as the basic prompt but with a newline afterwards
prompt :: String -> IO String
prompt str = do
    input <- inlinePrompt str 
    putStrLn ""
    return input
    

-- IO: Prefixes the prompt with one or more lines of instructions
multilinePrompt :: String -> [String] -> IO String
multilinePrompt promptName description = do
    putLines description
    prompt promptName
    
-- Print out multiple lines to the console
putLines :: [String] -> IO()
putLines [] = return ()
putLines (line:lines) = do
    putStrLn line
    putLines lines