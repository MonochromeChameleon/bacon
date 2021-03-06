{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module SearchParser(parseSearchResults, cleanedSearchResults) where

import Data.List
import Text.HTML.TagSoup

import DataModel
import Parser
import ParsingUtils

-- Public methods

-- | Extracts the list of Actors from a search results page
parseSearchResults :: String -> [Actor]
parseSearchResults = processHTML SearchParser 0


-- | Same as parseSearchResults, but with cleaned up names
cleanedSearchResults :: String -> [Actor]
cleanedSearchResults = processHTML SearchParser 1


-- Private methods

-- | Extracts the Actor details from a list of tags corresponding to a single row in our HTML page
-- | The bacon number passed in here is actually a boolean flag, which is hacky as hell, but it 
-- | does what we want.
doParseRow :: Bacon -> [Tag String] -> Actor
doParseRow bc tags = Actor { name = nm, actor_details = details }
    where tagsOfInterest = dropWhile notLink $ dropWhile notSearchResultCell tags -- Drop everything up to the hyperlink
          url = getLink $ tagsOfInterest!!0     -- URL comes first
          nm = if bc == 0 then getAllContent tagsOfInterest else getContent $ tagsOfInterest!!1
          imdbid = takeWhile (\x -> x /= '/') $ drop 6 url
          details = IMDBDetails { imdbId = imdbid, baconNumber = 0 }


-- Actual parser, as an instance of the EntityParser typeclass

data SearchParser = SearchParser

instance EntityParser SearchParser Actor where
    filterRows _ _ = True
    parseRow _ = doParseRow
    notRow _ = notEndTableRow
    
    {-  Filters required to retrieve the part of the html page that we are interested in, from an input list of 
    TagSoup tags. We can drop everything up until we reach a table with class 'cast_list', keeping everything
    from inside that table. The logic, therefore, is:
    - Drop all tags up to the table tag with class "class_list"
    - drop 1 for a reason I can't recall any longer, but which was required.
    - Keep everything up to the end of the table
    - Cleanup so that we start on a row  -}
    tagFilters _ = [dropWhile notSearchResultRow, takeWhile notEndTable]
    
