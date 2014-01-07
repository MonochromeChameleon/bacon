module BaconResult(getBaconResult) where

import Database.HDBC
import Text.JSON

import DatabaseConnector
import BaconDB
import DataModel
import ORM

-- | Looks up the given actor, returning a nested linkage of films back to the seed actor.
getBaconResult :: ImdbID -> IO JSValue
getBaconResult imdbId = do
    actor <- lookupActor imdbId
    result <- withConnection $ getBaconResult_ (bacon actor) imdbId
    return $ convertActor result


-- | Internal function called via DatabaseConnector.withConnection to get the linked actors and films
getBaconResult_ :: IConnection c => Bacon -> ImdbID -> c -> IO [SqlValue]
getBaconResult_ bc imdbId conn = do
    let query = buildQuery bc
    res <- quickQuery' conn query [toSql imdbId]
    return $ res!!0


-- | Converts the SQL response to a JSON actor
convertActor :: [SqlValue] -> JSValue
convertActor [] = JSObject $ toJSObject []
convertActor sqlValues = JSObject $ toJSObject [jsid, jsname, jsfilm]
    where jsid = ("imdbId", showJSON ((fromSql $ sqlValues!!0)::String))
          jsname = ("name", showJSON ((fromSql $ sqlValues!!1)::String))
          jsfilm = ("film", convertFilm $ drop 2 sqlValues)
          

-- | Converts the SQL response to a JSON film
convertFilm :: [SqlValue] -> JSValue
convertFilm [] = JSObject $ toJSObject []
convertFilm sqlValues = JSObject $ toJSObject [jsid, jstitle, jsyear, jsactor]
    where jsid = ("imdbId", showJSON ((fromSql $ sqlValues!!0)::String))
          jstitle = ("title", showJSON ((fromSql $ sqlValues!!1)::String))
          jsyear = ("year", showJSON ((fromSql $ sqlValues!!2)::String))
          jsactor = ("actor", convertActor $ drop 3 sqlValues)


-- | Constructs our (surprisingly convoluted) query according to the desired degree of bacon
buildQuery :: Bacon -> String
buildQuery bc = (selectPart bc) ++ (fromPart bc) ++ (joinPart bc) ++ " where a0.bacon = 0 and a" ++ (show bc) ++ ".actor_id = ?"


-- | Build up the select part of our query
selectPart :: Bacon -> String
selectPart 0 = "select a0.actor_id, a0.name"
selectPart bc = (selectPart $ bc - 1) ++ ", " ++ filmSelect ++ ", " ++ actorSelect
    where filmSelect = "f" ++ bcf ++ ".film_id, f" ++ bcf ++ ".title, f" ++ bcf ++ ".year"
          actorSelect = "a" ++ bca ++ ".actor_id, a" ++ bca ++ ".name"
          bcf = show $ bc - 1
          bca = show bc


-- | Build up the from part of our query
fromPart :: Bacon -> String
fromPart 0 = " from actor a0"
fromPart bc = (fromPart $ bc - 1) ++ " join actor_film af" ++ bc' ++ " join film f" ++ bc' ++ " join actor_film af" ++ bc' ++ "a join actor a" ++ (show bc) 
    where bc' = show $ bc - 1


-- | Build up the joins for our query
joinPart :: Bacon -> String
joinPart 0 = ""
joinPart 1 = " on a0.actor_id = af0.actor_id and f0.film_id = af0.film_id and f0.film_id = af0a.film_id and a1.actor_id = af0a.actor_id"
joinPart bc = (joinPart $ bc - 1) ++ " and a" ++ bc' ++ ".actor_id = af" ++ bc' ++ ".actor_id and f" ++ bc' ++ ".film_id = af" ++ bc' ++ ".film_id and f" ++ bc' ++ ".film_id = af" ++ bc' ++ "a.film_id and a" ++ (show $ bc) ++ ".actor_id = af" ++ bc' ++ "a.actor_id"
    where bc' = show $ bc - 1

