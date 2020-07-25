module SBrowser.Utils where

import qualified Data.List as DL
import SBrowser.FileInfo (FileInfo(..))

fileInfoString :: Maybe FileInfo -> String
fileInfoString Nothing = "<Invalid/No Selection>"
fileInfoString (Just fi) =
    desc fi ++
    if (desc fi) == "directory"
        then ""
        else ", " ++ (show $ size fi) ++ " bytes"
