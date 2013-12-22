{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module ActorParser(parseActor) where

import Text.HTML.TagSoup
import Data.List
import Data.Char
import Data.Text(strip, pack, unpack)

import DataModel
import StringUtils
import Parser

data ActorParser = ActorParser

instance EntityParser ActorParser Film where
    filterTags _ = filmographyTags
    filterRows _ = (filter notTVEtcRow)
    parseRow _ = doParseRow
    notRow _ = isNotRow

parseActor :: Bacon -> String -> [Film]
parseActor = processHTML ActorParser

          
-- Private methods

{-  Retrieves the part of the html page that we are interested in, from an input list of TagSoup tags. We can drop
everything up until we reach '<div id="filmo-head-act' (which may be actor or actress, hence the incomplete tag
search), and then we are interested in the next <div class="filmo-category-section">. The logic, therefore, is:
- Drop all tags up to the one whose id begins "filmo-head-act"
- Then drop all tags up to the next with class "filmo-category-section"
- Then keep all tags up until the next with id beginning "filmo-head"  -}

filmographyTags :: [Tag String] -> [Tag String]
filmographyTags tags = rowTags
    where tagsFromHeaderOnwards = dropWhile notActorHeader tags                   -- Find the appropriate header
          tagsFromFilmographyOnwards = dropWhile notSection tagsFromHeaderOnwards -- Find the corresponding content section
          filmographyTags = takeWhile notHeader $ dropWhile (not.notHeader) tagsFromFilmographyOnwards -- Get the content between headers
          rowTags = dropWhile isNotRow filmographyTags                              -- Cleanup so that we start on a row






doParseRow :: Bacon -> [Tag String] -> Film
doParseRow bc tags = Film { title = filmName, year = yr, film_details = details }
    where yearTags = dropWhile notYear tags
          yr = parseYear $ (unpack.strip.pack) $ getContent $ yearTags!!1
          tagsOfInterest = dropWhile notLink tags   -- We are only interested in the first <a href of each row, which will have the URL and the name
          url = getLink $ tagsOfInterest!!0         -- URL comes first
          filmName = getContent $ (dropWhile notTagText tagsOfInterest)!!0 -- Name of the film is the text node inside the <a>tag
          imdbid = takeWhile (\x -> x /= '/') $ drop 7 url
          details = IMDBDetails { imdbId = imdbid, baconNumber = bc }
          
parseYear :: String -> Year
parseYear "" = 2014
parseYear str = read (take 4 str)::Year

getLink :: Tag String -> String
getLink (TagOpen "a" atts) = snd $ (filter (\x -> fst x == "href") atts)!!0 -- Get the content of the href attribute
 
getContent :: Tag String -> String
getContent (TagText txt) = txt -- Get the text content of the node



-- Filtering functions for finding our tags of interest

notTagText :: Tag String -> Bool
notTagText (TagText _) = False
notTagText _ = True

notYear :: Tag String -> Bool
notYear (TagOpen tag atts) = length (filter (\x -> fst x == "class" && snd x == "year_column") atts) == 0
notYear _ = True

-- Check for a tag with id starting "filmo-head-act"
notActorHeader :: Tag String -> Bool
notActorHeader (TagOpen tag atts) = length (filter (\x -> fst x == "id" && (isPrefixOf "filmo-head-act" $ snd x)) atts) == 0
notActorHeader _ = True

-- Check for any tag with id starting "filmo-head-"
notHeader :: Tag String -> Bool
notHeader (TagOpen tag atts) = length (filter (\x -> fst x == "id" && (isPrefixOf "filmo-head-" $ snd x)) atts) == 0
notHeader _ = True

-- Check for any tag with class "filmo-category-section"
notSection :: Tag String -> Bool
notSection (TagOpen tag atts) = length (filter (\x -> fst x == "class" && snd x == "filmo-category-section") atts) == 0
notSection _ = True

-- Check for an anchor tag
notLink :: Tag String -> Bool
notLink (TagOpen tag _) = tag /= "a"
notLink _ = True
         
-- Check for an opening tag with class "filmo-row"   
isNotRow :: Tag String -> Bool
isNotRow (TagOpen tag atts) = length (filter (\x -> fst x == "class" && (isPrefixOf "filmo-row" $ snd x)) atts) == 0
isNotRow _ = True

notTVEtcRow :: [Tag String] -> Bool
notTVEtcRow tags = length (filter isTVTag tags) == 0

isTVTag :: Tag String -> Bool
isTVTag (TagText txt) = elem lowerText ["(tv series)", "(video short)", "(tv mini-series)", "(short)", "(tv movie)", "(video)", "(scenes deleted)", "(video game)", "(tv short)"] || isSuffixOf "(uncredited)" lowerText
    where lowerText = (unpack.strip.pack) $ map toLower txt
isTVTag _ = False

