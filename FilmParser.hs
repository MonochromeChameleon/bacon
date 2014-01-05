{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module FilmParser(parseFilm, checkAdultStatus) where

import Text.HTML.TagSoup
import Data.List
import Data.Text(strip, pack, unpack)

import DataModel
import Parser
import ParsingUtils

data FilmParser = FilmParser

instance EntityParser FilmParser Actor where
    filterRows _ = isCreditedCastRow
    parseRow _ = doParseRow
    notRow _ = notStartTableRow
    
    {-  Filters required to retrieve the part of the html page that we are interested in, from an input list of 
    TagSoup tags. We can drop everything up until we reach a table with class 'cast_list', keeping everything
    from inside that table. The logic, therefore, is:
    - Drop all tags up to the table tag with class "class_list"
    - drop 1 for a reason I can't recall any longer, but which was required.
    - Keep everything up to the end of the table
    - Cleanup so that we start on a row  -}
    tagFilters _ = [dropWhile notCastRow, drop 1, takeWhile notEndTable, dropWhile notStartTableRow]
    
parseFilm :: Bacon -> String -> [Actor]
parseFilm = processHTML FilmParser


doParseRow :: Bacon -> [Tag String] -> Actor
doParseRow bn tags = Actor { name = nm, actor_details = details }
    where tagsOfInterest = dropWhile notLink $ dropWhile notCastCell tags -- Drop everything up to the hyperlink
          url = getLink $ tagsOfInterest!!0     -- URL comes first
          nm = getContent $ tagsOfInterest!!3 -- Name of the person is the text node inside the <span> in the <a>tag
          imdbid = takeWhile (\x -> x /= '/') $ drop 6 url
          details = IMDBDetails { imdbId = imdbid, baconNumber = bn }
          





checkAdultStatus :: String -> Bool
checkAdultStatus html = doCheckAdultStatus tags
    where tags = parseTags html

          
-- Private methods



doCheckAdultStatus :: [Tag String] -> Bool
doCheckAdultStatus [] = False
doCheckAdultStatus tags = result
    where genreTags = dropWhile notGenreTag tags
          genreValue = getContent $ genreTags!!1
          isAdult = genreValue == "Adult"
          recursiveResult = doCheckAdultStatus $ drop 1 genreTags
          result = if length genreTags < 1 then False else (isAdult || recursiveResult)
    