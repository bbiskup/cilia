{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ui where

import Prelude
import qualified Data.Text as T
import Data.Maybe(fromMaybe)
import Control.Monad.IO.Class(liftIO)
import Data.List(sort)
import Lens.Micro((&), (^.), (.~))
import Lens.Micro.TH (makeLenses)
import Data.Time.Clock(getCurrentTime, UTCTime)
import Data.Time.Format(defaultTimeLocale, formatTime)
import qualified Graphics.Vty as V
import qualified Brick.Types as BT
import Brick.Util (on)
import Brick.Widgets.Core(
      txt
    , hBox
    , vBox
    , padTopBottom
    , withAttr
    )
import Brick.AttrMap (attrMap, AttrMap, AttrName)
import Brick.Main (
      App(..)
    , showFirstCursor
    , halt
    , continue
    )
import qualified Brick.Widgets.Center as BWC 
import Types( Repo
            ,  slug
            , lastBuildState
            , active
            , BuildState(..)
            , Conf
            , travisUser)

data AppState = 
    AppState { _conf :: Conf
             , _stLastVtyEvent :: Maybe V.Event 
             , _repos :: [Repo]
             , _timestamp :: UTCTime
             , _errMsg :: Maybe T.Text
}
    deriving (Eq, Show)

makeLenses ''AppState

getDateStr :: IO String
getDateStr = fmap show getCurrentTime

ui :: AppState -> [BT.Widget ()]
ui st =
    case (st ^. errMsg) of
        Nothing -> [vBox [headerUI st, repoUI activeRepos, statusBar']]
        (Just msg) -> [vBox [padTopBottom 1 (txt msg), statusBar']]
    where
        statusBar' = statusBar (st ^. timestamp)
        activeRepos = filter (\repo -> fromMaybe False (repo ^. active)) $ st ^. repos

headerUI :: AppState -> BT.Widget ()
headerUI st = BWC.hCenter $ headerParts 
    where
        headerTxt :: T.Text
        headerTxt = T.concat [ "Travis projects for "
                                   , st ^. conf . travisUser]
        headerParts = withAttr "status.normal" $ hBox 
            [ spaceFill'
            , txt headerTxt 
            , spaceFill'
            ]
            where spaceFill' = stretchHFill ' '

repoUI :: [Repo]-> BT.Widget ()
repoUI repos' 
    | not (null repos')  = vBox . fmap renderRepo . sort $ repos'
    | otherwise = txt "no repos" 
    where
        renderRepo repo = withAttr (colorRepo repo) $ hBox  
            [ txt " " 
            , txt . repoTxt $ repo
            , spaceFill'
            ]
            where spaceFill' = stretchHFill ' '

        repoTxt :: Repo -> T.Text
        repoTxt repo = T.concat 
            [ fromMaybe "-" (repo ^. slug)
            , " . " 
            , T.pack . show . fromMaybe Unknown $ repo ^. lastBuildState]

timestampTxt :: UTCTime -> T.Text
timestampTxt ts = T.pack $  formatTime defaultTimeLocale "%H:%m:%S" ts 

-- Fill horizontal space (custom widget)
stretchHFill :: Char -> BT.Widget n
stretchHFill ch = hBox[fWidget]
    where
        fWidget = BT.Widget BT.Greedy BT.Fixed $ do
            ctx <- BT.getContext
            let a = ctx ^. (BT.attrL)
            return $ BT.Result (V.charFill a ch (BT.availWidth ctx) 1) [] []

statusBar :: UTCTime -> BT.Widget ()
statusBar ts = withAttr "status.normal" $ hBox[
      spacer
    , stretchHFill ' '
    , txt $ timestampTxt ts
    , spacer
    ]
    where spacer = txt " " 

colorRepo :: Repo -> AttrName
colorRepo r = case buildState of
    Passed -> "build.passed"
    Failed -> "build.failed"
    Unknown -> "build.unknown"

    where buildState = fromMaybe Unknown (r ^. lastBuildState)

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("build.passed",      V.white `on` V.green)
    , ("build.failed",      V.white `on` V.red)
    , ("build.unknown",     V.white `on` V.black)
    , ("status.normal",     V.blue `on` V.white)
    ]

data CustomEvent = VtyEvent V.Event
                 | ReposUpdate [Repo]
                 | NetworkError T.Text


appEvent :: AppState -> CustomEvent -> BT.EventM () (BT.Next AppState)
appEvent st e = do
    timestamp' <- liftIO getCurrentTime
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent ev -> continue $ st & stLastVtyEvent .~ Just ev
        (ReposUpdate newRepos) -> continue $ st & (repos .~ newRepos) . (timestamp .~ timestamp') . (errMsg .~ Nothing)
        (NetworkError errMsg') -> continue $ st & (errMsg .~ (Just errMsg')) . (timestamp .~ timestamp')


app :: App AppState CustomEvent ()
app =
    App { appDraw = ui
        , appHandleEvent = appEvent
        , appAttrMap = const theMap
        , appStartEvent = return
        , appChooseCursor = showFirstCursor
        , appLiftVtyEvent = VtyEvent
        }

