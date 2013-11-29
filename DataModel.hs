module DataModel where

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
    
