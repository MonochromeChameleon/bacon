module DataModel where

import ORM
import Database.HDBC

-- | Shared details that are present for both actors and films
data IMDBDetails = IMDBDetails { imdbId :: ImdbID, baconNumber :: Bacon } deriving (Eq, Show, Read)

-- | Actor-specific fields
data Actor = Actor { name :: Name
                   , actor_details :: IMDBDetails } deriving (Eq, Show, Read)
                   
-- | Film-specific fields
data Film = Film { title :: Name
                 , year :: Year
                 , film_details :: IMDBDetails } deriving (Eq, Show)
                 
-- | Handy aliases
type ImdbID = String
type Name = String
type Year = Int
type Bacon = Int

-- Make our two main classes instances of the Entity typeclass, defining all of the ORM necessaries

instance Entity Actor where
    entityType _ = ActorType
    imdbid = imdbId.actor_details
    bacon = baconNumber.actor_details
    
    asSql actor = [toSql $ imdbid actor, toSql $ name actor, toSql $ bacon actor]
    readSql [id, nm, bc] = Actor { name = fromSql nm, actor_details = details }
        where details = IMDBDetails { imdbId = fromSql id, baconNumber = fromSql bc }


instance Entity Film where
    entityType _ = FilmType
    imdbid = imdbId.film_details
    bacon = baconNumber.film_details
    
    asSql film = [toSql $ imdbid film, toSql $ title film, toSql $ year film, toSql $ bacon film]
    readSql [id, nm, yr, bc] = Film { title = fromSql nm, year = fromSql yr, film_details = details }
        where details = IMDBDetails { imdbId = fromSql id, baconNumber = fromSql bc }


-- Build a numbered list of people - calls through to listNamesRec
listNames :: [Actor] -> [String]
listNames = listNamesRec 0 -- Call through to the recursive function with index 0

-- Recursive implementation of the listNames function
listNamesRec :: Integer -> [Actor] -> [String]
listNamesRec _ [] = []
listNamesRec ix (actor:actors) = ((show (ix + 1)) ++ ": " ++ (name actor)):listNamesRec (ix + 1) actors
    
