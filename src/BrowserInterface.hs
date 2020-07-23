{-# LANGUAGE OverloadedStrings #-}

module BrowserInterface where

import EntryCursor

import System.Directory
import System.Exit (die)

import Data.List (sort)
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Control.Monad.IO.Class (MonadIO, liftIO)

import Brick.AttrMap (attrMap)
import Brick.Main
import Brick.Types (BrickEvent(..), EventM, Next, ViewportType(..), Widget)
import Brick.Util (on)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core (emptyWidget, raw, str, vBox, viewport, withAttr)

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
        , entryCursor :: EntryCursor FilePath
        }
    deriving (Show, Eq)

data ResourceName =
    ListViewport
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
        , appAttrMap =
              const $
              attrMap
                  mempty
                  [ ("selected", GV.black `on` GV.white)
                  , ("title", GV.withStyle (GV.white `on` GV.blue) GV.bold)
                  , ("statusOK", GV.withStyle (GV.white `on` GV.green) GV.bold)
                  , ("statusNotOK", GV.withStyle (GV.white `on` GV.red) GV.bold)
                  ]
        }

initialize :: IO BrowserState
initialize = do
    cd <- getCurrentDirectory
    paths <- DL.sort <$> (getDirectoryContents cd)
    case NE.nonEmpty paths of
        Nothing -> die "Empty directory list"
        Just p' -> return BrowserState {status = OK, currDir = cd, entryCursor = makeCursor $ NE.toList p'}

render :: BrowserState -> [Widget ResourceName]
render s =
    let title = "sbrowse 0.1.0.0"
        eCurs = entryCursor s
     in flip (:) [] $
        vBox
            [ vBox
                  [ (withAttr "title") . hCenter . str $ title
                  , if status s == OK
                        then (withAttr "statusOK") . hCenter . str $ "Browsing" ++ currDir s
                        else (withAttr "statusNotOK") . hCenter . str $ (\(NotOK s) -> s) $ status s
                  ]
            , viewport ListViewport Vertical $
              vBox $
              concat
                  [ str <$> (reverse $ prevEntries eCurs)
                  , flip (:) [] $ (withAttr "selected") . str $ currEntry eCurs
                  , str <$> (nextEntries eCurs)
                  ]
            ]

handleEv :: BrowserState -> BrickEvent ResourceName e -> EventM ResourceName (Next BrowserState)
handleEv s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey KDown [] -> do
                    vScrollBy (viewportScroll ListViewport) 1
                    continue $ select nextEntry s
                EvKey KUp [] -> do
                    vScrollBy (viewportScroll ListViewport) (-1)
                    continue $ select prevEntry s
                EvKey KEnter [] -> do
                    s' <- liftIO $ enterDir s
                    if status s' == OK
                        then do
                            vScrollToBeginning (viewportScroll ListViewport)
                        else pure ()
                    continue s'
                _ -> continue s
        _ -> continue s

select :: (EntryCursor FilePath -> Maybe (EntryCursor FilePath)) -> BrowserState -> BrowserState
select d s =
    let eCurs = entryCursor s
     in case d eCurs of
            Nothing -> s
            Just e' -> s {status = OK, currDir = currDir s, entryCursor = e'}

enterDir :: BrowserState -> IO BrowserState
enterDir s = do
    let eCurs = entryCursor s
    isDirectory <- doesDirectoryExist $ currEntry eCurs
    isReadable <- ((getPermissions $ currEntry eCurs) >>= (\p -> pure $ readable p))
    if isReadable
        then do
            if isDirectory
                then do
                    setCurrentDirectory $ currEntry eCurs
                    s' <- initialize
                    pure s'
                else pure $
                     s
                         { status = NotOK $ "Cannot open: '" ++ (currEntry eCurs) ++ "' is a file"
                         , currDir = currDir s
                         , entryCursor = eCurs
                         }
        else pure $
             s
                 { status = NotOK $ "Cannot open '" ++ (currEntry eCurs) ++ "': permission denied"
                 , currDir = currDir s
                 , entryCursor = eCurs
                 }
