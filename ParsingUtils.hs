module ParsingUtils where

import Text.HTML.TagSoup
import Data.List
import Data.Char
import Data.Text(strip, pack, unpack)

import DataModel

-- | Utilities for filtering and parsing HTML pages, used in the various implementations of the
-- | EntityParser typeclass

-- ======================= --
-- BASIC PARSING UTILITIES --
-- ======================= --

-- | Returns the text content of a TagText tag
getContent :: Tag String -> String
getContent (TagText txt) = txt -- Get the text content of the node
getContent _ = ""


-- | Returns all of the text contents from a list of tags
getAllContent :: [Tag String] -> String
getAllContent [] = ""
getAllContent (tag:tags) = getContent tag ++ (getAllContent tags)


-- | Allow filtering on non-text tags
notTagText :: Tag String -> Bool
notTagText (TagText _) = False
notTagText _ = True


-- | Returns whether the specified tag has the given id
hasId :: String -> Tag String -> Bool
hasId idName (TagOpen tag atts) = length (filter (\x -> fst x == "id" && (isInfixOf idName $ snd x)) atts) > 0
hasId _ _ = False


-- | Returns whether the specified tag has the given class
hasClass :: String -> Tag String -> Bool
hasClass className (TagOpen tag atts) = length (filter (\x -> fst x == "class" && (isInfixOf className $ snd x)) atts) > 0
hasClass _ _ = False


-- | Returns whether the specified tag is an opening tag of the specified type
isOpenTag :: String -> Tag String -> Bool
isOpenTag tagType (TagOpen open atts) = open == tagType
isOpenTag _ _ = False


-- | Returns whether the specified tag is a closing tag of the specified type
isCloseTag :: String -> Tag String -> Bool
isCloseTag tagType (TagClose close) = tagType == close
isCloseTag _ _ = False


-- =============================== --
-- MORE SPECIFIC PARSING UTILITIES --
-- =============================== --

-- | Allow filtering on the opening of a table row
notStartTableRow :: Tag String -> Bool
notStartTableRow = not.(isOpenTag "tr")


-- | Allow filtering on the closing of a table row
notEndTableRow :: Tag String -> Bool
notEndTableRow = not.(isCloseTag "tr")


-- | Allow filtering on the opening of an anchor tag
notLink :: Tag String -> Bool
notLink = not.(isOpenTag "a")

-- | Allow filtering on the start of a table
notStartTable :: Tag String -> Bool
notStartTable = not.(isOpenTag "table")


-- | Allow filtering on the end of a table
notEndTable :: Tag String -> Bool
notEndTable = not.(isCloseTag "table")


-- | Extract the content of the href attribute from an anchor tag
getLink :: Tag String -> String
getLink (TagOpen "a" atts) = snd $ (filter (\x -> fst x == "href") atts)!!0 -- Get the content of the href attribute
getLink _ = "" 


-- =============================== --
-- VERY SPECIFIC PARSING UTILITIES --
-- =============================== --


-- Actor page utils

-- | Allow filtering on Actor filmography rows = check for "filmo-row" class
isNotFilmographyRow :: Tag String -> Bool
isNotFilmographyRow = not.(hasClass "filmo-row")


-- | Allow filtering on filmography year tags
isNotFilmYear :: Tag String -> Bool
isNotFilmYear = not.(hasClass "year_column")


-- | Parse a string to an integer year, defaulting to 2014
parseYear :: String -> Year
parseYear "" = 2014
parseYear str = read (take 4 str)::Year


-- | Allow filtering on whether the tag is the start of a filmography section
notFilmographyHeader :: Tag String -> Bool
notFilmographyHeader = not.(hasId "filmo-head-")


-- | Allow filtering on whether the tag is the start of the filmography as an actor (i.e. not producer etc.)
notActorFilmographyHeader :: Tag String -> Bool
notActorFilmographyHeader = not.(hasId "filmo-head-act")


-- | Allow filtering on whether the tag is the start of a filmography section
notFilmographySection :: Tag String -> Bool
notFilmographySection = not.(hasClass "filmo-category-section")


-- | Allow filtering on filmography rows that correspond to entries we want to disregard
notIgnoredFilmographyRow :: [Tag String] -> Bool
notIgnoredFilmographyRow tags = length (filter shouldIgnore tags) == 0


-- | Indicates whether the given tag is one that corresponds to a film we want to disregard
shouldIgnore :: Tag String -> Bool
shouldIgnore (TagText txt) = elem lowerText ["(tv series)", "(video short)", "(tv mini-series)", "(short)", "(tv movie)", "(video)", "(scenes deleted)", "(video game)", "(tv short)"] || isSuffixOf "(uncredited)" lowerText
    where lowerText = (unpack.strip.pack) $ map toLower txt
shouldIgnore _ = False


-- Film page utils

-- | Allow filtering on whether the given tag is the start of a cast list
notCastRow :: Tag String -> Bool
notCastRow tag = isNotTable || isNotCastList
    where isNotTable = notStartTable tag
          isNotCastList = not (hasClass "cast_list" tag)


-- | Allow filtering on whether the given tag is a genre tag
notGenreTag :: Tag String -> Bool
notGenreTag (TagOpen tag atts) = tag /= "span" || length (filter (\x -> fst x == "class" && (snd x == "itemprop")) atts) == 0 || length (filter (\x -> fst x == "itemprop" && (snd x == "genre")) atts) == 0
notGenreTag _ = True


-- | Allow filtering of uncredited cast members
isCreditedCastRow :: [Tag String] -> Bool
isCreditedCastRow tags = length (dropWhile notCastCell tags) > 0 && length (filter (\x -> isSuffixOf "(uncredited)" $ (unpack.strip.pack) $ getContent x) tags) == 0


-- | Allow filtering on whether the given tag is an actor cell in the full cast list
notCastCell :: Tag String -> Bool
notCastCell (TagOpen tag atts) = length (filter (\x -> fst x == "itemprop" && (snd x == "actor")) atts) == 0
notCastCell _ = True


-- Search page utils

-- | Allow filtering on whether the given row is a search result row
notSearchResultRow :: Tag String -> Bool
notSearchResultRow = not.(hasClass "findResult")


-- | Allow filtering on whether the given tag is the actual search result
notSearchResultCell :: Tag String -> Bool
notSearchResultCell = not.(hasClass "result_text")
