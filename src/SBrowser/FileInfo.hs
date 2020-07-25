module SBrowser.FileInfo where

import Data.Time.Clock
import Magic.Data
import Magic.Init
import Magic.Operations
import System.Directory

data FileInformation =
    FileInformation
        { size :: Integer
        , lastAccessTime :: UTCTime
        , lastModifyTime :: UTCTime
        , mimeType :: String
        , encoding :: String
        }
    deriving (Show, Eq)

getFileInformation :: FilePath -> IO FileInformation
getFileInformation fp = do
    at <- getAccessTime fp
    mt <- getModificationTime fp
    sz <- getFileSize fp
    mi <- flip magicFile fp =<< (\m -> magicLoadDefault m >> return m) =<< magicOpen [MagicMimeType, MagicMimeEncoding]
    return $ FileInformation sz at mt (getMimeType mi) (getMimeEncoding mi)

getMimeType :: String -> String
getMimeType = takeWhile (\c -> c /= ';')

getMimeEncoding :: String -> String
getMimeEncoding = (drop 10) . dropWhile (\c -> c /= ';')
