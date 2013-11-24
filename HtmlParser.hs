module HtmlParser where

import StringUtils

notOpenTag :: Char -> Bool
notOpenTag x = x /= '<'

notCloseTag :: Char -> Bool
notCloseTag x = x /= '>'

startingTagName :: String -> String
startingTagName [] = ""
startingTagName ('<':xs) = takeWhile notWhiteSpace $ takeWhile notCloseTag xs
startingTagName (_:_) = ""

extractNode :: String -> String
extractNode str = magic 0 (startingTagName str) str

grabDiv :: String -> (String, String)
grabDiv = doGrabDiv 0 ""

grabTag :: String -> String -> (String, String)
grabTag tagName = doGrabTag tagName 0 ""

doGrabTag :: String -> Int -> String -> String -> (String, String)
doGrabTag tagName depth grabbed str | depth == 1 && (startsWith closeTag str) = (grabbed ++ closeTag, drop (length closeTag) str)
                                    | startsWith closeTag str                 = doGrabTag tagName (depth - 1) newGrabbed rest
                                    | startsWith openTag str                  = doGrabTag tagName (depth + 1) newGrabbed rest
                                    | otherwise                               = doGrabTag tagName depth       newGrabbed rest
    where closeTag   = "</" ++ tagName ++ ">"
          openTag    = "<" ++ tagName
          newGrabbed = grabbed ++ [head str]
          rest       = tail str

doGrabDiv :: Int -> String -> String -> (String, String)
doGrabDiv depth grabbed (x:xs) | depth == 1 && (startsWith "</div>" (x:xs)) = (grabbed ++ "</div>", drop 5 xs)
                               | startsWith "</div>" (x:xs) = doGrabDiv (depth - 1) (grabbed ++ "<") xs
                               | startsWith "<div" (x:xs) = doGrabDiv (depth + 1) (grabbed ++ "<") xs
                               | otherwise = doGrabDiv depth (grabbed ++ [x]) xs


toNextTag str = dropWhile notOpenTag str





magic :: Int -> String -> String -> String
magic depth tagName str | startingTagName nextTag == '/':tagName = stringTo ("</" ++ tagName ++ ">") str
                        | otherwise = magic (depth + 1) (startingTagName nextTag) nextTag
    where nextTag = toNextTag str


