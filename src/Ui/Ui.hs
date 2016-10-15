{-# LANGUAGE OverloadedStrings #-}

module Ui where

import qualified Data.Text as T
import Data.Monoid((<>))
import Data.Text.Markup((@@))

import qualified Graphics.Vty as V
import qualified Brick.Types as BT
import Brick.Markup(markup, (@?))
import Brick.Util (on, fg)
import Brick.Widgets.Core(
      (<+>)
    , (<=>)
    , str
    )
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Main (App(..), defaultMain, resizeOrQuit, neverShowCursor)


ui :: BT.Widget ()
ui = str "Hello"


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.yellow)
    , ("keyword2",      V.white `on` V.blue)
    ]

data AppState = 
    AppState { travisUser :: T.Text }
    deriving (Eq, Show)

app :: App AppState V.Event ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appAttrMap = const theMap
        , appStartEvent = return
        , appChooseCursor = neverShowCursor
        , appLiftVtyEvent = id
        }

