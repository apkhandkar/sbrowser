module SBrowser.Utils where

import qualified Data.List as DL
import Data.Time.Clock
import Data.Time.Format
import SBrowser.FileInfo (FileInfo(..))

fileInfoString :: Maybe FileInfo -> String
fileInfoString Nothing = "<Invalid/No Selection>"
fileInfoString (Just fi) =
    desc fi ++
    if (desc fi) == "directory"
        then ""
        else ", " ++ (show $ size fi) ++ " bytes"

timeToString :: UTCTime -> String
timeToString = formatTime (defaultTimeLocale) "%r %Z, %d-%m-%Y"
