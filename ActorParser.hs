{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module ActorParser(parseActor) where

import Text.HTML.TagSoup
import Data.List
import Data.Char
import Data.Text(strip, pack, unpack)

import DataModel
import Parser
import ParsingUtils

data ActorParser = ActorParser

instance EntityParser ActorParser Film where
    filterRows _ = notIgnoredFilmographyRow
    parseRow _ = doParseRow
    notRow _ = isNotFilmographyRow
    
    {-  Filters required to retrieve the part of the html page that we are interested in, from an input list of 
    TagSoup tags. We can drop everything up until we reach '<div id="filmo-head-act' (which may be actor or 
    actress, hence the incomplete tag search), and then we are interested in the next
    <div class="filmo-category-section">. The logic, therefore, is:
    - Drop all tags up to the one whose id begins "filmo-head-act"
    - Then drop all tags up to the next with class "filmo-category-section"
    - Then keep all tags up until the next with id beginning "filmo-head"  -}
    tagFilters _ = [dropWhile notActorFilmographyHeader, dropWhile notFilmographySection, dropWhile (not.notFilmographyHeader), takeWhile notFilmographyHeader, dropWhile isNotFilmographyRow]

parseActor :: Bacon -> String -> [Film]
parseActor = processHTML ActorParser

          
-- Private methods


doParseRow :: Bacon -> [Tag String] -> Film
doParseRow bc tags = Film { title = filmName, year = yr, film_details = details }
    where yearTags = dropWhile isNotFilmYear tags
          yr = parseYear $ (unpack.strip.pack) $ getContent $ yearTags!!1
          tagsOfInterest = dropWhile notLink tags   -- We are only interested in the first <a href of each row, which will have the URL and the name
          url = getLink $ tagsOfInterest!!0         -- URL comes first
          filmName = getContent $ (dropWhile notTagText tagsOfInterest)!!0 -- Name of the film is the text node inside the <a>tag
          imdbid = takeWhile (\x -> x /= '/') $ drop 7 url
          details = IMDBDetails { imdbId = imdbid, baconNumber = bc }
          
