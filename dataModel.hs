module DataModel where

data Actor = Actor { actor_id :: ImdbID
                   , name :: Name
                   , baconNumber :: BaconNumber } deriving (Eq, Show)
                   
data Film = Film { film_id :: ImdbID
                 , title :: Name
                 , year :: Year } deriving (Eq, Show)
                 
type ImdbID = String
type Name = String
type Year = Int
type BaconNumber = Int

type URL = String
          
convertActorDetails :: BaconNumber -> [(Name, ImdbID)] -> [Actor]
convertActorDetails baconNumber details = map (createActor baconNumber) details

createActor :: BaconNumber -> (Name, ImdbID) -> Actor
createActor bn (nm, imdb) = Actor { actor_id = imdb, name = nm, baconNumber = bn }