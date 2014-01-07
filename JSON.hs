module JSON where

import Text.JSON
import DataModel
import ORM

instance JSON Actor where
    readJSON _ = Error "not implemented" -- We don't need to read from JSON, so skip implementation
    showJSON a = JSObject $ toJSObject [jsname, jsid, jsbacon]
        where jsname = toJSTuple "name" $ name a
              jsid = toJSTuple "imdbId" $ imdbid a
              jsbacon = toJSTuple "bacon" $ show $ bacon a
    

instance JSON Film where
    readJSON _ = Error "not implemented" -- We don't need to read from JSON, so skip implementation
    showJSON f = JSObject $ toJSObject [jstitle, jsyear, jsid, jsbacon]
        where jstitle = toJSTuple "title" $ title f
              jsyear = toJSTuple "year" $ show $ year f
              jsid = toJSTuple "imdbId" $ imdbid f
              jsbacon = toJSTuple "bacon" $ show $ bacon f


toJSTuple :: String -> String -> (String, JSValue)
toJSTuple name value = (name, JSString $ toJSString value)