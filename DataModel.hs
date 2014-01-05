module DataModel where

import ORM
import Database.HDBC

data IMDBDetails = IMDBDetails { imdbId :: ImdbID, baconNumber :: Bacon } deriving (Eq, Show, Read)

data Actor = Actor { name :: Name
                   , actor_details :: IMDBDetails } deriving (Eq, Show, Read)
                   
data Film = Film { title :: Name
                 , year :: Year
                 , film_details :: IMDBDetails } deriving (Eq, Show)
                 
type ImdbID = String
type Name = String
type Year = Int
type Bacon = Int


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



