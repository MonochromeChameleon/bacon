{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Parser where

import Text.HTML.TagSoup

import DataModel
import ORM

class (Entity e) => EntityParser p e | p -> e where
    processHTML :: p -> Bacon -> String -> [e]
    processHTML parser bacon html  = details
        where tags           = parseTags html
              tagsOfInterest = filterTags parser tags
              rows           = groupByRows parser tagsOfInterest
              rowsOfInterest = filterRows parser rows
              details        = map (parseRow parser bacon) rowsOfInterest
              
    filterTags :: p -> [Tag String]   -> [Tag String]
    filterRows :: p -> [[Tag String]] -> [[Tag String]]
    parseRow :: p -> Bacon -> [Tag String] -> e
    notRow :: p -> Tag String -> Bool


    -- Takes a list of tags and groups those tags by the row that they are contained in
    groupByRows :: p -> [Tag String] -> [[Tag String]]
    groupByRows self tags = doGroup self [] tags


    -- Use recursive descent parsing to get the rows from our tag list
    doGroup :: p -> [[Tag String]] -> [Tag String] -> [[Tag String]]
    doGroup _ groups [] = groups
    doGroup self groups (tag:tags) = doGroup self (newGroup:groups) rest
        where (newGroup, rest) = span (notRow self) tags
