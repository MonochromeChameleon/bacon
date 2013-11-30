module DataModel where

import ORM
import Database.HDBC

data Actor = Actor { actor_id :: ImdbID
                   , name :: Name
                   , bacon :: Bacon } deriving (Eq, Show, Read)
                   
data Film = Film { film_id :: ImdbID
                 , title :: Name
                 , year :: Year } deriving (Eq, Show)
                 
type ImdbID = String
type Name = String
type Year = Int
type Bacon = Int

type URL = String



          
convertActorDetails :: Bacon -> [(Name, ImdbID)] -> [Actor]
convertActorDetails bacon details = map (createActor bacon) details

createActor :: Bacon -> (Name, ImdbID) -> Actor
createActor bn (nm, imdb) = Actor { actor_id = imdb, name = nm, bacon = bn }


imdbBaseUrl :: String
imdbBaseUrl = "http://www.imdb.com/"

actorUrl :: Actor -> URL
actorUrl actor = imdbBaseUrl ++ "name/" ++ (actor_id actor) ++ "/"

filmUrl :: Film -> URL
filmUrl film = imdbBaseUrl ++ "title/" ++ (film_id film) ++ "/"

fullCastUrl :: Film -> URL
fullCastUrl film = imdbBaseUrl ++ "title/" ++ (film_id film) ++ "/fullcredits"
    




instance Entity Actor where
    tableName _ = "actor"
    idColumnName _ = "actor_id"
    allColumns _ = "(actor_id, name, bacon)"
    imdbid = actor_id
    readSql = readActorSql
    asSql = actorAsSql
    
readActorSql :: [SqlValue] -> Actor
readActorSql [id, nm, bc, _] = Actor { actor_id = fromSql id, name = fromSql nm, bacon = fromSql bc }

actorAsSql :: Actor -> [SqlValue]
actorAsSql actor = [toSql $ imdbid actor, toSql $ name actor, toSql $ bacon actor]
        


instance Entity Film where
    tableName _ = "film"
    idColumnName _ = "film_id"
    allColumns _ = "(film_id, title, year)"
    imdbid = film_id
    readSql = readFilmSql
    asSql = filmAsSql

readFilmSql :: [SqlValue] -> Film
readFilmSql [id, nm, yr, _] = Film { film_id = fromSql id, title = fromSql nm, year = fromSql yr }

filmAsSql :: Film -> [SqlValue]
filmAsSql film = [toSql $ imdbid film, toSql $ title film, toSql $ year film]


