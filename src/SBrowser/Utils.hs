module SBrowser.Utils where

import qualified Data.List as DL
import SBrowser.FileInfo (FileInformation(..))

printFileInfo :: Maybe FileInformation -> String
printFileInfo Nothing = "<Invalid/No Selection>"
printFileInfo (Just fi) =
    if DL.isInfixOf "directory" (mimeType fi)
        then "Directory ..."
        else show (size fi) ++ " bytes, MIME Type: " ++ mimeType fi ++ ", Encoding: " ++ encoding fi
