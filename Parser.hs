{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Parser where

import Text.HTML.TagSoup

import DataModel
import ORM

 
getContent :: Tag String -> String
getContent (TagText txt) = txt -- Get the text content of the node
getContent _ = ""

-- | Recursively applies filters to a list of tags
filterTags :: [([Tag String] -> [Tag String])] -> [Tag String] -> [Tag String]
filterTags [] tags = tags
filterTags (f:filts) tags = filterTags filts (f tags)

groupByRows :: (Tag String -> Bool) -> [Tag String] -> [[Tag String]]
groupByRows groupFunc tags = doGroup groupFunc [] tags

doGroup :: (Tag String -> Bool) -> [[Tag String]] -> [Tag String] -> [[Tag String]]
doGroup func groups tags = doGroup func (newGroup:groups) rest --QQ (tag:tags???)
    where (newGroup, rest) = span func tags

class (Entity e) => EntityParser p e | p -> e where
    processHTML :: p -> Bacon -> String -> [e]
    processHTML parser bacon html  = details
        where tags           = parseTags html
              tagsOfInterest = filterTags (tagFilters parser) tags
              rows           = groupByRows (notRow parser) tagsOfInterest
              rowsOfInterest = filter (filterRows parser) rows
              details        = map (parseRow parser bacon) rowsOfInterest
              
    filterRows :: p -> [Tag String] -> Bool
    parseRow :: p -> Bacon -> [Tag String] -> e
    notRow :: p -> Tag String -> Bool
    
    tagFilters :: p -> [([Tag String] -> [Tag String])]    
    