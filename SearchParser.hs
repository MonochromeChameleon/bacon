{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module SearchParser where

import Data.List
import Text.HTML.TagSoup

import DataModel
import Parser

data SearchParser = SearchParser

instance EntityParser SearchParser Actor where
    filterRows _ _ = True
    parseRow _ = doParseRow
    notRow _ = isNotRow
    
    {-  Filters required to retrieve the part of the html page that we are interested in, from an input list of 
    TagSoup tags. We can drop everything up until we reach a table with class 'cast_list', keeping everything
    from inside that table. The logic, therefore, is:
    - Drop all tags up to the table tag with class "class_list"
    - drop 1 for a reason I can't recall any longer, but which was required.
    - Keep everything up to the end of the table
    - Cleanup so that we start on a row  -}
    tagFilters _ = [dropWhile notResultRow, takeWhile notEndTable]
    
parseSearchResults :: String -> [Actor]
parseSearchResults = processHTML SearchParser 0



isNotRow :: Tag String -> Bool
isNotRow (TagClose tag) = tag /= "tr"
isNotRow _ = True


notEndTable :: Tag String -> Bool
notEndTable (TagClose "table") = False
notEndTable _ = True


notResultRow :: Tag String -> Bool
notResultRow (TagOpen tag atts) = length (filter (\x -> fst x == "class" && (isPrefixOf "findResult" $ snd x)) atts) == 0
notResultRow _ = True



notResultCell :: Tag String -> Bool
notResultCell (TagOpen tag atts) = length ((filter (\x -> fst x == "class" && snd x == "result_text")) atts) == 0
notResultCell _ = True


doParseRow :: Bacon -> [Tag String] -> Actor
doParseRow _ tags = Actor { name = nm, actor_details = details }
    where tagsOfInterest = dropWhile notLink $ dropWhile notResultCell tags -- Drop everything up to the hyperlink
          url = getLink $ tagsOfInterest!!0     -- URL comes first
          nm = getAllContent $ tagsOfInterest
          imdbid = takeWhile (\x -> x /= '/') $ drop 6 url
          details = IMDBDetails { imdbId = imdbid, baconNumber = 0 }



-- Check for an anchor tag
notLink :: Tag String -> Bool
notLink (TagOpen tag _) = tag /= "a"
notLink _ = True



getLink :: Tag String -> String
getLink (TagOpen "a" atts) = snd $ (filter (\x -> fst x == "href") atts)!!0 -- Get the content of the href attribute
