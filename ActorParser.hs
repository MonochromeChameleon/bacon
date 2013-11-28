module ActorParser where --(getFilmographyDetails) where

import Text.HTML.TagSoup
import Data.List
import Data.Char

import DataModel
import StringUtils

-- Only one public method: getFilmographyDetails, which parses an html page into a list of (film name, url) tuples

getFilmographyDetails :: String -> [Film]
getFilmographyDetails html = filmDetails
    where tags        = parseTags html          -- TagSoup parsing
          filmography = filmographyTags tags    -- Get rid of anything that isn't the acting filmography section (i.e. not director, soundtrack etc.)
          rows        = groupByRows filmography -- Group the filmography into rows
          filmRows    = filter notTVEtcRow rows -- Remove TV Shows and suchlike (they have big casts and it just gets too huge!)
          filmDetails = map parseRow filmRows   -- Parse our rows into the necessary details

          
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
          rowTags = dropWhile notRow filmographyTags                              -- Cleanup so that we start on a row


-- The filmography is split into divs with class "filmo-row odd" and "filmo-row even". This function
-- takes a list of tags and groups those tags by the row that they are contained in
groupByRows :: [Tag String] -> [[Tag String]]
groupByRows tags = doGroup [] tags


-- Use recursive descent parsing to get the rows from our tag list
doGroup :: [[Tag String]] -> [Tag String] -> [[Tag String]]
doGroup groups [] = groups
doGroup groups (tag:tags) = doGroup (newGroup:groups) rest
   where (newGroup, rest) = span notRow tags



parseRow :: [Tag String] -> Film
parseRow tags = Film { title = filmName, film_id = imdbId, year = yr }
    where yearTags = dropWhile notYear tags
          yr = parseYear $ trim $ getContent $ yearTags!!1
          tagsOfInterest = dropWhile notLink tags   -- We are only interested in the first <a href of each row, which will have the URL and the name
          url = getLink $ tagsOfInterest!!0         -- URL comes first
          filmName = getContent $ (dropWhile notTagText tagsOfInterest)!!0 -- Name of the film is the text node inside the <a>tag
          imdbId = takeWhile (\x -> x /= '/') $ drop 7 url
          
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
notActorHeader (TagOpen tag atts) = length (filter (\x -> fst x == "id" && (startsWith "filmo-head-act" $ snd x)) atts) == 0
notActorHeader _ = True

-- Check for any tag with id starting "filmo-head-"
notHeader :: Tag String -> Bool
notHeader (TagOpen tag atts) = length (filter (\x -> fst x == "id" && (startsWith "filmo-head-" $ snd x)) atts) == 0
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
notRow :: Tag String -> Bool
notRow (TagOpen tag atts) = length (filter (\x -> fst x == "class" && (startsWith "filmo-row" $ snd x)) atts) == 0
notRow _ = True

notTVEtcRow :: [Tag String] -> Bool
notTVEtcRow tags = length (filter isTVTag tags) == 0

isTVTag :: Tag String -> Bool
isTVTag (TagText txt) = elem lowerText ["(tv series)", "(video short)", "(tv mini-series)", "(short)", "(tv movie)", "(video)", "(scenes deleted)", "(video game)", "(tv short)"] || endsWith "(uncredited)" lowerText
    where lowerText = trim $ map toLower txt
isTVTag _ = False

