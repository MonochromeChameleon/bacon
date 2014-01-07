{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Server where

import Control.Monad.IO.Class
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.JSON
import Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Char8 as L

import BaconDB
import BaconResult
import DataModel
import JSON
import Search

-- | Starts our HTTP server
runServer :: IO ()
runServer = do
    Prelude.putStrLn "Open up a browser and go to http://localhost:8000"
    serve Nothing baconApp


-- | Basic routing configuration
baconApp :: ServerPart Response
baconApp = msum [dir "degrees" $ path $ \q -> getDegrees q, dir "search"  $ path $ \q -> doSearch q, fileServing]


-- | Looks up the degrees of kevin bacon for the given imdb id, retuning a JSON tree
getDegrees :: String -> ServerPart Response
getDegrees q = do
    result <- liftIO $ getBaconResult q
    let json = encode $ showJSON result
    let html = toHtml $ json
    ok $ toResponseBS (C.pack "text/json") (L.pack json)
    

-- | Query the IMDB search page and return the results as a JSON array
doSearch :: String -> ServerPart Response
doSearch q = do
    actors <- liftIO $ searchResults q
    let json = encode $ Prelude.map showJSON $ fst actors
    let html = toHtml $ json
    ok $ toResponseBS (C.pack "text/json") (L.pack json)


-- | Serve the static directory
fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["index.html"] "./static"