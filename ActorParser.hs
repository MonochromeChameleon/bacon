module ActorParser where

import HtmlParser

extractFilmography :: String -> String
extractFilmography = stringFrom "<div id=\"filmography\">"

