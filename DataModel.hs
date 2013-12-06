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

type URL = String

dummyFilm :: Film
dummyFilm = Film { title = [], year = -1, film_details = dummyDetails }

dummyActor :: Actor
dummyActor = Actor { name = [], actor_details = dummyDetails }

dummyDetails :: IMDBDetails
dummyDetails = IMDBDetails { imdbId = [], baconNumber = -1 }

          
convertActorDetails :: Bacon -> [(Name, ImdbID)] -> [Actor]
convertActorDetails bacon details = map (createActor bacon) details

createActor :: Bacon -> (Name, ImdbID) -> Actor
createActor bn (nm, imdb) = Actor { name = nm, actor_details = details }
   where details = IMDBDetails { imdbId = imdb, baconNumber = bn }


imdbBaseUrl :: String
imdbBaseUrl = "http://www.imdb.com/"

actorUrl :: Actor -> URL
actorUrl actor = imdbBaseUrl ++ "name/" ++ (imdbid actor) ++ "/"

filmUrl :: Film -> URL
filmUrl film = imdbBaseUrl ++ "title/" ++ (imdbid film) ++ "/"

fullCastUrl :: Film -> URL
fullCastUrl film = imdbBaseUrl ++ "title/" ++ (imdbid film) ++ "/fullcredits"
    




instance Entity Actor where
    tableName _ = "actor"
    idColumnName _ = "actor_id"
    allColumns _ = "actor_id, name, bacon"
    imdbid = imdbId.actor_details
    bacon = baconNumber.actor_details
    readSql = readActorSql
    asSql = actorAsSql
    
readActorSql :: [SqlValue] -> Actor
readActorSql [id, nm, bc] = Actor { name = fromSql nm, actor_details = details }
    where details = IMDBDetails { imdbId = fromSql id, baconNumber = fromSql bc }

actorAsSql :: Actor -> [SqlValue]
actorAsSql actor = [toSql $ imdbid actor, toSql $ name actor, toSql $ bacon actor]
        


instance Entity Film where
    tableName _ = "film"
    idColumnName _ = "film_id"
    allColumns _ = "film_id, title, year, bacon"
    imdbid = imdbId.film_details
    bacon = baconNumber.film_details
    readSql = readFilmSql
    asSql = filmAsSql

readFilmSql :: [SqlValue] -> Film
readFilmSql [id, nm, yr, bc] = Film { title = fromSql nm, year = fromSql yr, film_details = details }
    where details = IMDBDetails { imdbId = fromSql id, baconNumber = fromSql bc }

filmAsSql :: Film -> [SqlValue]
filmAsSql film = [toSql $ imdbid film, toSql $ title film, toSql $ year film, toSql $ bacon film]


