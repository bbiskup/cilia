{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ui where

import Prelude
import qualified Data.Text as T
import Data.Maybe(fromMaybe)
import Data.List(sort)
import Lens.Micro((&), (^.), (.~))
import Lens.Micro.TH (makeLenses)
import qualified Graphics.Vty as V
import qualified Brick.Types as BT
import Brick.Markup(markup, (@?))
import Brick.Util (on)
import Brick.Widgets.Core(
      txt
    , vBox
    )
import Brick.AttrMap (attrMap, AttrMap, AttrName)
import Brick.Main (
      App(..)
    , showFirstCursor
    , halt
    , continue
    )
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
             }
    deriving (Eq, Show)

makeLenses ''AppState


ui :: AppState -> [BT.Widget ()]
ui st = [vBox [hello, repoUI activeRepos]]
    where 
        activeRepos = filter (\repo -> fromMaybe False (repo ^. active)) $ st ^. repos
        hello = txt $ T.concat [ "Hello "
                               , st ^. conf . travisUser]

repoUI :: [Repo]-> BT.Widget ()
repoUI repos' 
    | not (null repos')  = vBox . fmap renderRepo . sort $ repos'
    | otherwise = txt "no repos" 
    where
        renderRepo repo =  markup (repoTxt repo @? colorRepo repo)

        repoTxt :: Repo -> T.Text
        repoTxt repo = T.concat 
            [ fromMaybe "-" (repo ^. slug)
            , " . " 
            , T.pack . show . fromMaybe Unknown $ repo ^. lastBuildState]

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
    ]

data CustomEvent = VtyEvent V.Event
                 | ReposUpdate [Repo]


appEvent :: AppState -> CustomEvent -> BT.EventM () (BT.Next AppState)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent ev -> continue $ st & stLastVtyEvent .~ Just ev
        (ReposUpdate newRepos) -> continue $ st & repos .~ newRepos


app :: App AppState CustomEvent ()
app =
    App { appDraw = ui
        , appHandleEvent = appEvent
        , appAttrMap = const theMap
        , appStartEvent = return
        , appChooseCursor = showFirstCursor
        , appLiftVtyEvent = VtyEvent
        }

