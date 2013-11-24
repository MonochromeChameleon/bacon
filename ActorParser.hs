module ActorParser(getFilmographyDetails) where

import Text.HTML.TagSoup
import Data.List

import StringUtils

-- Only one public method: getFilmographyDetails, which parses an html page into a list of (film name, url) tuples

getFilmographyDetails :: String -> [(String, String)]
getFilmographyDetails html = filmDetails
    where tags        = parseTags html          -- TagSoup parsing
          filmography = filmographyTags tags    -- Get rid of anything that isn't the acting filmography section (i.e. not director, soundtrack etc.)
          rows        = groupByRows filmography -- Group the filmography into rows
          filmDetails = map parseRow rows       -- Parse our rows into the necessary details

          
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
          filmographyTags = takeWhile notHeader $ tail tagsFromFilmographyOnwards -- Drop the tail from the content of interest
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



parseRow :: [Tag String] -> (String, String)
parseRow tags = (filmName, url)
    where tagsOfInterest = dropWhile notLink tags   -- We are only interested in the first <a href of each row, which will have the URL and the name
          url = getLink $ tagsOfInterest!!0         -- URL comes first
          filmName = getContent $ tagsOfInterest!!1 -- Name of the film is the text node inside the <a>tag

getLink :: Tag String -> String
getLink (TagOpen "a" atts) = snd $ (filter (\x -> fst x == "href") atts)!!0 -- Get the content of the href attribute
 
getContent :: Tag String -> String
getContent (TagText txt) = txt -- Get the text content of the node



-- Filtering functions for finding our tags of interest

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
