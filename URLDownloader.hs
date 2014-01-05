module URLDownloader(downloadURL) where

import Language.Haskell.TH.Ppr -- cabal install template-haskell
import Network.HTTP.Conduit -- cabal install http-conduit
import Data.Word
import Data.Either
import Control.Exception
import qualified Data.ByteString.Lazy as L

-- ============================ --
-- == PAGE DOWNLOAD FUNCTION == --
-- ============================ --

-- | Downloads the given URL, returning the HTML content of the page, or else failing after four attempts
downloadURL :: String -> IO String
downloadURL = tryDownloadURL 0

-- Private function

-- | Recursively keep attempting downloads until either success, or too many failed attempts, in
-- | which case return an empty string. Uses ByteString processing to optimize the 
-- | request/response processing time
tryDownloadURL :: Integer -> String -> IO String
tryDownloadURL 4 _ = return ""
tryDownloadURL attempts url = do
    result <- try (simpleHttp url) :: IO (Either HttpException L.ByteString)
    let e = lefts [result]
    if length e == 0 then do
        let text_bs = head.rights $ [result]
        let html_word8 = L.unpack text_bs :: [Word8]
        return $ bytesToString html_word8 :: IO String
    else do
        putStrLn $ "Download Error: " ++ url
        -- Recurse until we get a result, or fail too many times
        tryDownloadURL (attempts + 1) url
