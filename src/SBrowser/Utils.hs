module SBrowser.Utils where

import qualified Data.List as DL
import Data.Time.Clock
import Data.Time.Format
import SBrowser.FileInfo (FileInfo(..))

fileInfoString :: Maybe FileInfo -> String
fileInfoString Nothing = "<Invalid/No Selection>"
fileInfoString (Just fi) =
    (shortenDesc $ desc fi) ++
    if (desc fi) == "directory"
        then ""
        else ", " ++ (show $ size fi) ++ " bytes"

timeString :: Maybe FileInfo -> String
timeString Nothing = ""
timeString (Just fi) = "LAT " ++ (timeToString $ lastAccessTime fi) ++ ", LMT: " ++ (timeToString $ lastModifyTime fi)

timeToString :: UTCTime -> String
timeToString = formatTime (defaultTimeLocale) "%r %Z, %d-%m-%Y"

-- Iffy way to shorten the description returned by libmagic
-- Ignore everything after a comma, colon or semicolon
shortenDesc :: String -> String
shortenDesc = takeWhile $ \c -> (c /= ',') && (c /= ':') && (c /= ';')
