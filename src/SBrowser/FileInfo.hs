module SBrowser.FileInfo where

import Data.Time.Clock
import Magic.Data
import Magic.Init
import Magic.Operations
import System.Directory

data FileInfo =
    FileInfo
        { size :: Integer
        , lastAccessTime :: UTCTime
        , lastModifyTime :: UTCTime
        , desc :: String
        }
    deriving (Show, Eq)

getFileInfo :: FilePath -> IO FileInfo
getFileInfo fp = do
    size <- getFileSize fp
    acct <- getAccessTime fp
    modt <- getModificationTime fp
    desc <- flip magicFile fp =<< (\m -> magicLoadDefault m >> return m) =<< magicOpen [MagicNone]
    return $ FileInfo size acct modt desc
