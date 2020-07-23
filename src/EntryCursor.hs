module EntryCursor where

data EntryCursor a =
    EntryCursor
        { prevEntries :: [a]
        , currEntry :: a
        , nextEntries :: [a]
        }
    deriving (Show, Eq)

makeCursor :: (Show a, Eq a) => [a] -> EntryCursor a
makeCursor li = EntryCursor {prevEntries = [], currEntry = head li, nextEntries = tail li}

nextEntry :: (Eq a) => EntryCursor a -> Maybe (EntryCursor a)
nextEntry e
    | nextEntries e == [] = Nothing
    | otherwise =
        Just
            EntryCursor
                { prevEntries = (currEntry e) : (prevEntries e)
                , currEntry = head $ nextEntries e
                , nextEntries = tail $ nextEntries e
                }

prevEntry :: (Eq a) => EntryCursor a -> Maybe (EntryCursor a)
prevEntry e
    | prevEntries e == [] = Nothing
    | otherwise =
        Just
            EntryCursor
                { prevEntries = tail $ prevEntries e
                , currEntry = head $ prevEntries e
                , nextEntries = (currEntry e) : (nextEntries e)
                }
