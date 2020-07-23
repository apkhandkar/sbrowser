{-# LANGUAGE OverloadedStrings #-}

module BrowserInterface where

import System.Directory
import System.Exit (die)

import Data.List (sort)
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V (fromList)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Brick.AttrMap (AttrMap, attrMap)
import Brick.Main
import Brick.Types (BrickEvent(..), EventM, Next, ViewportType(..), Widget, vpSize)
import Brick.Util (on)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core (emptyWidget, raw, str, vBox, viewport, withAttr)
import qualified Brick.Widgets.List as L

import qualified Graphics.Vty as GV
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

data Status
    = OK
    | NotOK String
    deriving (Show, Eq)

data BrowserState =
    BrowserState
        { status :: Status
        , currDir :: FilePath
        , dirList :: L.List ResourceName FilePath
        }
    deriving (Show)

data ResourceName
    = ListViewport
    | DirList
    deriving (Show, Eq, Ord)

runBrowser :: IO ()
runBrowser = do
    initState <- initialize
    finalState <- defaultMain browserInterface initState
    pure ()

browserInterface :: App BrowserState e ResourceName
browserInterface =
    App
        { appDraw = render
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEv
        , appStartEvent = pure
        , appAttrMap = const attributes
        }

attributes :: AttrMap
attributes =
    attrMap
        GV.defAttr
        [ (L.listSelectedAttr, GV.black `on` GV.white)
        , ("title", GV.withStyle (GV.white `on` GV.blue) GV.bold)
        , ("statusOK", GV.withStyle (GV.white `on` GV.green) GV.bold)
        , ("statusNotOK", GV.withStyle (GV.white `on` GV.red) GV.bold)
        ]

initialize :: IO BrowserState
initialize = do
    cd <- getCurrentDirectory
    paths <- DL.sort <$> (getDirectoryContents cd)
    if null paths
        then die "Empty directory list"
        else return $ BrowserState OK cd (L.list DirList (V.fromList paths) 1)

render :: BrowserState -> [Widget ResourceName]
render s =
    let title = "sbrowse 0.1.0.0"
     in flip (:) [] $
        vBox
            [ vBox
                  [ (withAttr "title") . hCenter . str $ title
                  , if status s == OK
                        then (withAttr "statusOK") . hCenter . str $ "Browsing" ++ currDir s
                        else (withAttr "statusNotOK") . hCenter . str $ (\(NotOK s) -> s) $ status s
                  ]
            , (L.renderList listDrawElement True (dirList s))
            ]

listDrawElement :: Bool -> FilePath -> Widget ResourceName
listDrawElement s e = str $ e

handleEv :: BrowserState -> BrickEvent ResourceName e -> EventM ResourceName (Next BrowserState)
handleEv s e = do
    case e of
        VtyEvent ev ->
            case ev of
                EvKey (KChar 'q') [] -> halt s
                EvKey KEnter [] -> do
                    s' <- liftIO $ enterDir s
                    continue s'
                ev -> do
                    e <- L.handleListEvent ev (dirList s)
                    continue s {dirList = e}
        _ -> continue s

enterDir :: BrowserState -> IO BrowserState
enterDir s = do
    let mbSelection = L.listSelectedElement $ dirList s
    case mbSelection of
        Nothing -> return s
        Just (_, currEntry) -> do
            isDirectory <- doesDirectoryExist currEntry
            isReadable <- ((getPermissions $ currEntry) >>= (\p -> pure $ readable p))
            if isReadable
                then do
                    if isDirectory
                        then do
                            setCurrentDirectory $ currEntry
                            s' <- initialize
                            pure s'
                        else pure $ s {status = NotOK $ "Cannot open: '" ++ currEntry ++ "' is a file"}
                else pure $ s {status = NotOK $ "Cannot open '" ++ currEntry ++ "': permission denied"}
