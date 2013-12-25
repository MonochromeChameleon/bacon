{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module FilmParser(parseFilm, checkAdultStatus) where

import Text.HTML.TagSoup
import Data.List
import Data.Text(strip, pack, unpack)

import DataModel
import Parser

data FilmParser = FilmParser

instance EntityParser FilmParser Actor where
    filterRows _ = isCreditedCastRow
    parseRow _ = doParseRow
    notRow _ = isNotRow
    
    {-  Filters required to retrieve the part of the html page that we are interested in, from an input list of 
    TagSoup tags. We can drop everything up until we reach a table with class 'cast_list', keeping everything
    from inside that table. The logic, therefore, is:
    - Drop all tags up to the table tag with class "class_list"
    - drop 1 for a reason I can't recall any longer, but which was required.
    - Keep everything up to the end of the table
    - Cleanup so that we start on a row  -}
    tagFilters _ = [dropWhile notCast, drop 1, takeWhile notEndTable, dropWhile isNotRow]
    
parseFilm :: Bacon -> String -> [Actor]
parseFilm = processHTML FilmParser

--QQ

isNotRow :: Tag String -> Bool
isNotRow (TagOpen "tr" atts) = False
isNotRow _ = True

--
notCast :: Tag String -> Bool
notCast (TagOpen tag atts) = tag /= "table" || length (filter (\x -> fst x == "class" && (snd x == "cast_list")) atts) == 0
notCast _ = True

notEndTable :: Tag String -> Bool
notEndTable (TagClose "table") = False
notEndTable _ = True





-- Only one public method: getCastDetails, which parses an html page into a list of (name, url) tuples

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
    
notGenreTag :: Tag String -> Bool
notGenreTag (TagOpen tag atts) = tag /= "span" || length (filter (\x -> fst x == "class" && (snd x == "itemprop")) atts) == 0 || length (filter (\x -> fst x == "itemprop" && (snd x == "genre")) atts) == 0
notGenreTag _ = True


doParseRow :: Bacon -> [Tag String] -> Actor
doParseRow bn tags = Actor { name = nm, actor_details = details }
    where tagsOfInterest = dropWhile notLink $ dropWhile notCastCell tags -- Drop everything up to the hyperlink
          url = getLink $ tagsOfInterest!!0     -- URL comes first
          nm = getContent $ tagsOfInterest!!3 -- Name of the person is the text node inside the <span> in the <a>tag
          imdbid = takeWhile (\x -> x /= '/') $ drop 6 url
          details = IMDBDetails { imdbId = imdbid, baconNumber = bn }
          

-- Check for an anchor tag
notLink :: Tag String -> Bool
notLink (TagOpen tag _) = tag /= "a"
notLink _ = True



getLink :: Tag String -> String
getLink (TagOpen "a" atts) = snd $ (filter (\x -> fst x == "href") atts)!!0 -- Get the content of the href attribute



-- Filtering functions for finding our tags of interest

isCreditedCastRow :: [Tag String] -> Bool
isCreditedCastRow tags = length (dropWhile notCastCell tags) > 0 && length (filter (\x -> isSuffixOf "(uncredited)" $ (unpack.strip.pack) $ getContent x) tags) == 0

notCastCell :: Tag String -> Bool
notCastCell (TagOpen tag atts) = length (filter (\x -> fst x == "itemprop" && (snd x == "actor")) atts) == 0
notCastCell _ = True

