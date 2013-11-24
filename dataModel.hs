module DataModel where

type Actor = ImdbRecord
type Film = ImdbRecord

data ImdbRecord = ImdbRecord { imdbId :: ImdbID
                             , name :: Name
                             , baconNumber :: BaconNumber } deriving (Eq, Show)
                 
type ImdbID = String
type Name = String
type BaconNumber = Int

type URL = String


convertDetails :: BaconNumber -> [(Name, ImdbID)] -> [ImdbRecord]
convertDetails baconNumber details = map (createRecord baconNumber) details

createRecord :: BaconNumber -> (Name, ImdbID) -> ImdbRecord
createRecord bn (nm, imdb) = ImdbRecord { baconNumber = bn, name = nm, imdbId = imdb}
          
