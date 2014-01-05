{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Parser where

import Text.HTML.TagSoup

import DataModel
import ORM

 
-- | Recursively applies filters to a list of tags
filterTags :: [([Tag String] -> [Tag String])] -> [Tag String] -> [Tag String]
filterTags [] tags = tags
filterTags (f:filts) tags = filterTags filts (f tags)


-- | Group tags into lists of tags corresponding to rows in our html page
groupByRows :: (Tag String -> Bool) -> [Tag String] -> [[Tag String]]
groupByRows groupFunc tags = doGroup groupFunc [] tags


-- | Recursive function to handle the grouping
doGroup :: (Tag String -> Bool) -> [[Tag String]] -> [Tag String] -> [[Tag String]]
doGroup _ groups [] = groups
doGroup _ groups (tag:[]) = groups
doGroup func groups (tag:tags) = doGroup func (groups ++ [newGroup]) rest
    where (newGroup, rest) = span func tags


-- | Defines the html parsing process for all our individual page parsers. I think I might have done too
-- | much Java programming as I'm kind of thinking of this as an Interface with default implementations,
-- | which maybe isn't the Haskelly way to do things, but it works and I don't like code repetition.
class (Entity e) => EntityParser p e | p -> e where
    -- | Actual parsing function
    processHTML :: p -> Bacon -> String -> [e]
    processHTML parser bacon html  = details
        where tags           = parseTags html
              tagsOfInterest = filterTags (tagFilters parser) tags
              rows           = groupByRows (notRow parser) tagsOfInterest
              rowsOfInterest = filter (filterRows parser) rows
              details        = map (parseRow parser bacon) rowsOfInterest

    -- | Implementation-specific parsing functions
    filterRows :: p -> [Tag String] -> Bool
    parseRow :: p -> Bacon -> [Tag String] -> e
    notRow :: p -> Tag String -> Bool
    
    -- | The list of filters that should be applied to extract the part of the page we are interested in
    tagFilters :: p -> [([Tag String] -> [Tag String])]    
