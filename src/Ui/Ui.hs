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
import Data.Time.Format.Human as TFH
import qualified Graphics.Vty as V
import qualified Brick.Types as BT
import Brick.Util (on)
import Brick.Markup(markup, (@?))
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
            , lastBuildFinishedAt
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

padTxtRight :: Int -> T.Text -> T.Text
padTxtRight n t  = T.pack $ s ++ p
  where p = replicate n ' '
        s = T.unpack t



ui :: AppState -> [BT.Widget ()]
ui st =
    case st ^. errMsg of
        Nothing -> [vBox [ headerUI st
                         , repoUI st activeRepos
                         , statusBar'
                         ]
                    ]
        (Just msg) -> [ vBox [ padTopBottom 1 (txt msg)
                             , statusBar'
                             ]
                      ]
    where
        statusBar' = statusBar (st ^. timestamp)
        activeRepos = filter (\repo -> fromMaybe False (repo ^. active)) $ st ^. repos

headerUI :: AppState -> BT.Widget ()
headerUI st = BWC.hCenter  headerParts 
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

repoUI :: AppState -> [Repo]-> BT.Widget ()
repoUI st repos' 
    | not (null repos')  = vBox . fmap renderRepo . sort $ repos'
    | otherwise = txt "no repos" 
    where
        renderRepo repo =  hBox  
            [ txt " " 
            , txt . padTxtRight (maxSlugLen - T.length slug')  $ slug'
            , txt " "
            , colorBuildState $ repo ^. lastBuildState
            , txt " "
            , txt $  lastBuildFinishedTxt st repo
            , spaceFill'
            ]
            where spaceFill' = stretchHFill ' '
                  maxSlugLen = maximum . fmap (T.length . fromMaybe "-" . (^. slug)) $ repos'
                  slug' = fromMaybe "-" $ repo ^. slug

timestampTxt :: UTCTime -> T.Text
timestampTxt ts = T.pack $  formatTime defaultTimeLocale "%H:%m:%S" ts 

lastBuildFinishedTxt :: AppState -> Repo -> T.Text
lastBuildFinishedTxt st repo = case repo ^. lastBuildFinishedAt of
    Nothing ->  T.pack "<unknown time>"
    (Just t) -> T.pack $ TFH.humanReadableTime' (st ^. timestamp) t

-- Fill horizontal space (custom widget)
stretchHFill :: Char -> BT.Widget n
stretchHFill ch = hBox[fWidget]
    where
        fWidget = BT.Widget BT.Greedy BT.Fixed $ do
            ctx <- BT.getContext
            let a = ctx ^. BT.attrL
            return $ BT.Result (V.charFill a ch (BT.availWidth ctx) 1) [] []

statusBar :: UTCTime -> BT.Widget ()
statusBar ts = withAttr "status.normal" $ hBox[
      spacer
    , txt "ESC to quit"
    , stretchHFill ' '
    , txt $ timestampTxt ts
    , spacer
    ]
    where spacer = txt " " 

colorBuildState :: Maybe BuildState -> BT.Widget ()
colorBuildState maybeBuildState = 
    let attr = case buildState of
                    Passed -> "build.passed"
                    Failed -> "build.failed"
                    Unknown -> "build.unknown"
    in
        markup ((T.pack . show $ buildState) @? attr)
    where buildState = fromMaybe Unknown maybeBuildState

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
        (ReposUpdate newRepos) -> continue $ st 
            & (repos .~ newRepos) 
            . (timestamp .~ timestamp') 
            . (errMsg .~ Nothing)
        (NetworkError errMsg') -> continue $ st 
            & (errMsg .~ Just errMsg') 
            . (timestamp .~ timestamp')


app :: App AppState CustomEvent ()
app =
    App { appDraw = ui
        , appHandleEvent = appEvent
        , appAttrMap = const theMap
        , appStartEvent = return
        , appChooseCursor = showFirstCursor
        , appLiftVtyEvent = VtyEvent
        }

