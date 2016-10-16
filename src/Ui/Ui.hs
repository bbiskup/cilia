{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ui where

import qualified Data.Text as T
import Data.Maybe(fromMaybe)
import Data.Monoid((<>))
import Data.Text.Markup((@@))
import Lens.Micro((&), (^.), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import qualified Graphics.Vty as V
import qualified Brick.Types as BT
import Brick.Markup(markup, (@?))
import Brick.Util (on, fg)
import Brick.Widgets.Core(
      (<+>)
    , (<=>)
    , txt
    , vBox
    )
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Main (
      App(..)
    , defaultMain
    , resizeOrQuit
    , showFirstCursor
    , halt
    , continue
    )
import Types(Repo,  slug, description)

data AppState = 
    AppState { _travisUser :: T.Text
             , _stLastVtyEvent :: Maybe V.Event 
             , _repos :: [Repo]
             }
    deriving (Eq, Show)

makeLenses ''AppState


ui :: AppState -> [BT.Widget ()]
ui st = [vBox [hello, repoUI $ st ^. repos]]
    where hello = txt $ T.concat [ "Hello "
                                  , st ^. travisUser]

repoUI :: [Repo]-> BT.Widget ()
repoUI repos 
    | length repos > 0 = 
        let repo = repos !! 0 in
            renderRepo repo
    | otherwise = txt "no repos" 
    where
        renderRepo repo = txt $ T.concat 
            [ fromMaybe "-" (repo ^. slug)
            , " . " 
            , fromMaybe "-" (repo ^. description)]

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.yellow)
    , ("keyword2",      V.white `on` V.blue)
    ]

data CustomEvent = VtyEvent V.Event
                 | ReposUpdate [Repo]


appEvent :: AppState -> CustomEvent -> BT.EventM () (BT.Next AppState)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent ev -> continue $ st & stLastVtyEvent .~ (Just ev)
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

