{-# LANGUAGE OverloadedStrings #-}

module Ui where

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


ui :: BT.Widget ()
ui = str "Hello"


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.yellow)
    , ("keyword2",      V.white `on` V.blue)
    ]
