{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module FilmParser(parseFilm, checkAdultStatus) where

import Text.HTML.TagSoup
import Data.List
import Data.Text(strip, pack, unpack)

import DataModel
import Parser
import StringUtils

data FilmParser = FilmParser

instance EntityParser FilmParser Actor where
    filterTags _ = castTags
    filterRows _ = (filter isCreditedCastRow)
    parseRow _ = doParseRow
    notRow _ = isNotRow
    
parseFilm :: Bacon -> String -> [Actor]
parseFilm = processHTML FilmParser

-- Only one public method: getCastDetails, which parses an html page into a list of (name, url) tuples

checkAdultStatus :: String -> Bool
checkAdultStatus html = doCheckAdultStatus tags
    where tags = parseTags html

          
-- Private methods

{-  Retrieves the part of the html page that we are interested in, from an input list of TagSoup tags. We can drop
everything up until we reach '<div id="filmo-head-act' (which may be actor or actress, hence the incomplete tag
search), and then we are interested in the next <div class="filmo-category-section">. The logic, therefore, is:
- Drop all tags up to the one whose id begins "filmo-head-act"
- Then drop all tags up to the next with class "filmo-category-section"
- Then keep all tags up until the next with id beginning "filmo-head"  -}

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

castTags :: [Tag String] -> [Tag String]
castTags tags = rowTags
    where tagsFromCastOnwards = dropWhile notCast tags                -- Find the start of the cast table
          castTags = takeWhile notEndTable $ drop 1 tagsFromCastOnwards -- Drop the tail from the content of interest
          rowTags = dropWhile isNotRow castTags                         -- Cleanup so that we start on a row


doParseRow :: Bacon -> [Tag String] -> Actor
doParseRow bn tags = Actor { name = nm, actor_details = details }
    where tagsOfInterest = dropWhile notLink $ dropWhile notCastCell tags -- Drop everything up to the hyperlink
          url = getLink $ tagsOfInterest!!0     -- URL comes first
          nm = getContent $ tagsOfInterest!!3 -- Name of the person is the text node inside the <span> in the <a>tag
          imdbid = takeWhile (\x -> x /= '/') $ drop 6 url
          details = IMDBDetails { imdbId = imdbid, baconNumber = bn }

getLink :: Tag String -> String
getLink (TagOpen "a" atts) = snd $ (filter (\x -> fst x == "href") atts)!!0 -- Get the content of the href attribute
 
getContent :: Tag String -> String
getContent (TagText txt) = txt -- Get the text content of the node
getContent _ = ""



-- Filtering functions for finding our tags of interest

isCreditedCastRow :: [Tag String] -> Bool
isCreditedCastRow tags = length (dropWhile notCastCell tags) > 0 && length (filter (\x -> isSuffixOf "(uncredited)" $ (unpack.strip.pack) $ getContent x) tags) == 0

notCastCell :: Tag String -> Bool
notCastCell (TagOpen tag atts) = length (filter (\x -> fst x == "itemprop" && (snd x == "actor")) atts) == 0
notCastCell _ = True

notCast :: Tag String -> Bool
notCast (TagOpen tag atts) = tag /= "table" || length (filter (\x -> fst x == "class" && (snd x == "cast_list")) atts) == 0
notCast _ = True

notEndTable :: Tag String -> Bool
notEndTable (TagClose "table") = False
notEndTable _ = True

isNotRow :: Tag String -> Bool
isNotRow (TagOpen "tr" atts) = False
isNotRow _ = True

-- Check for an anchor tag
notLink :: Tag String -> Bool
notLink (TagOpen tag _) = tag /= "a"
notLink _ = True

