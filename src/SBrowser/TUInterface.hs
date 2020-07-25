{-# LANGUAGE OverloadedStrings #-}

module SBrowser.TUInterface where

import Brick.AttrMap (AttrMap, attrMap)
import Brick.Main
import Brick.Types (BrickEvent(..), EventM, Next, ViewportType(..), Widget, vpSize)
import Brick.Util (on)
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core (emptyWidget, padLeftRight, raw, str, vBox, viewport, withAttr)
import qualified Brick.Widgets.List as WL
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (sort)
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as DV (fromList)
import qualified Graphics.Vty as GV
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import SBrowser.FileInfo
import System.Directory
import System.Exit (die)

versionString :: String
versionString = "0.1.0.0"

data Status
    = OK
    | NotOK String
    deriving (Show, Eq)

data BrowserState =
    BrowserState
        { status :: Status
        , currDir :: FilePath
        , dirList :: WL.List ResourceName FilePath
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
        [ (WL.listSelectedAttr, GV.black `on` GV.white)
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
        else return $ BrowserState OK cd (WL.list DirList (DV.fromList paths) 1)

render :: BrowserState -> [Widget ResourceName]
render s =
    flip (:) [] $
    borderWithLabel (padLeftRight 2 $ str $ title) $
    vBox
        [ vBox
              [ if status s == OK
                    then (withAttr "statusOK") . hCenter . str $ "Browsing " ++ currDir s
                    else (withAttr "statusNotOK") . hCenter . str $ (\(NotOK s) -> s) $ status s
              ]
        , WL.renderList (\s e -> str $ e) True (dirList s)
        ]
  where
    title = "sbrowse v" ++ versionString

handleEv :: BrowserState -> BrickEvent ResourceName e -> EventM ResourceName (Next BrowserState)
handleEv s e = do
    case e of
        VtyEvent ev ->
            case ev of
                EvKey (KChar 'q') [] -> halt s
                EvKey KEnter [] -> continue =<< (liftIO $ enterDir s)
                ev -> continue =<< handleNavigation s ev
        _ -> continue s

handleNavigation :: BrowserState -> Event -> EventM ResourceName BrowserState
handleNavigation s e = WL.handleListEventVi WL.handleListEvent e (dirList s) >>= \l -> return $ s {dirList = l}

enterDir :: BrowserState -> IO BrowserState
enterDir s = do
    let mbSelection = WL.listSelectedElement $ dirList s
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
