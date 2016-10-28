{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cilia.Ui where

import Prelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF
import Data.Maybe(fromMaybe)
import Control.Monad.IO.Class(liftIO)
import Data.List(sortBy)
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
    , withAttr
    )
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Main (
      App(..)
    , showFirstCursor
    , halt
    , continue
    )
import qualified Brick.Widgets.Center as BWC 
import Cilia.CI.InternalRepo(
    InternalRepo
  , BuildState(..)
  , slug
  , lastBuildState
  , lastBuildFinishedAt
  , lastBuildDuration
  , active
  , BuildState(..))
import Cilia.Config(Config, travis, userName)

type ErrorMsg = T.Text

data AppState = 
    AppState { _conf :: Config
             , _stLastVtyEvent :: Maybe V.Event 
             , _repos :: [InternalRepo]
             , _timestamp :: UTCTime
             , _errMsg :: Maybe ErrorMsg
}
    deriving (Eq, Show)

makeLenses ''AppState

getDateStr :: IO String
getDateStr = show <$> getCurrentTime

padTxtRight :: Int -> T.Text -> T.Text
padTxtRight n t  = T.pack $ s ++ p
  where p = replicate n ' '
        s = T.unpack t



ui :: AppState -> [BT.Widget ()]
ui st = [vBox [ headerUI st
                         , repoUI st activeRepos
                         , statusBar'
                         ]
                    ]
    where
        statusBar' = statusBar 
                        (st ^. timestamp) 
                        (badStatusCount activeRepos)
                        (st ^. errMsg)
        activeRepos = filter (\repo -> fromMaybe False (repo ^. active)) $ st ^. repos

headerUI :: AppState -> BT.Widget ()
headerUI st = BWC.hCenter  headerParts 
    where
        headerTxt :: T.Text
        headerTxt = T.concat [ "Travis projects for "
                             , st ^. conf . travis . userName]
        headerParts = withAttr "status.normal" $ hBox 
            [ spaceFill'
            , txt headerTxt 
            , spaceFill'
            ]
            where spaceFill' = stretchHFill ' '

repoUI :: AppState -> [InternalRepo]-> BT.Widget ()
repoUI st repos' 
    | not (null repos')  = vBox . fmap renderRepo . sortBy cmpLastBuildFinishedAt $ repos'
    | otherwise = txt "no repos" 
    where
        cmpLastBuildFinishedAt x y =
            compare (y ^. lastBuildFinishedAt) (x ^. lastBuildFinishedAt)
        renderRepo repo =  hBox  
            [ txt " "
            , txt . padTxtRight (maxSlugLen - T.length slug')  $ slug'
            , spacer
            , colorBuildState $ repo ^. lastBuildState
            , spacer
            , txt .  padTxtRight (18 - T.length lastBuildFinishedTxt') $ lastBuildFinishedTxt'
            , spacer
            , txt lastBuildDurationTxt
            , spaceFill'
            ]
            where spacer = txt "  "
                  spaceFill' = stretchHFill ' '
                  maxSlugLen = maximum . fmap (T.length . fromMaybe "-" . (^. slug)) $ repos'
                  slug' = fromMaybe "-" $ repo ^. slug
                  lastBuildDurationTxt = fromMaybe " " (humanizeDuration <$> (repo ^. lastBuildDuration))
                  lastBuildFinishedTxt' = lastBuildFinishedTxt st repo

humanizeDuration :: Int -> T.Text
humanizeDuration d = TL.toStrict formatted
    where formatted
            | d > 86400   = TF.format "{} days" (TF.Only $ d `div` 86400)
            | d > 3600    = TF.format "{} hours" (TF.Only $ d `div` 3600)
            | d > 60      = TF.format "{} minutes" (TF.Only $ d `div` 60)
            | otherwise   = TF.format "{} seconds" (TF.Only d)


badStatusCount :: [InternalRepo] -> Int
badStatusCount = length . filter isNotPassed
    where isNotPassed repo =    
            let buildState = fromMaybe Unknown $ repo ^. lastBuildState in
            (buildState == Failed) || (buildState == Error)


timestampTxt :: UTCTime -> T.Text
timestampTxt ts = T.pack $  formatTime defaultTimeLocale "%H:%m:%S" ts 

lastBuildFinishedTxt :: AppState -> InternalRepo -> T.Text
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

statusBar :: UTCTime -> Int -> Maybe ErrorMsg -> BT.Widget ()
statusBar ts numFailed maybeErrMsg = withAttr "status.normal" $ hBox[
      spacer
    , txt "ESC to quit"
    , stretchHFill ' '
    , markup (noPassCountMsg @? noPassCountMsgAttr)
    , spacer
    , errorPart
    , spacer
    , txt $ timestampTxt ts
    , spacer
    ]
    where spacer = txt " "
          noPassCountMsg = TL.toStrict . TF.format "[{} failed/errored]" $ TF.Only numFailed
          noPassCountMsgAttr
            | numFailed > 0 = "status.error"
            | otherwise = "status.normal"
          errorPart = case maybeErrMsg of
                Nothing -> txt ""
                (Just _) -> markup ("[network error]" @? "status.error")

colorBuildState :: Maybe BuildState -> BT.Widget ()
colorBuildState maybeBuildState = 
    let attr = case buildState of
                    Passed -> "build.passed"
                    Failed -> "build.failed"
                    Error -> "build.error"
                    Queued -> "build.queued"
                    Running -> "build.running"
                    Unknown -> "build.unknown"
    in
        markup (bsTxt @? attr)
    where buildState = fromMaybe Unknown maybeBuildState
          bsT = T.pack $ show $ buildState
          bsTxt = padTxtRight (7 - T.length bsT) bsT


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("build.passed",      V.black `on` V.green)
    , ("build.failed",      V.white `on` V.red)
    , ("build.error",     V.red `on` V.white)
    , ("build.queued",     V.black `on` V.white)
    , ("build.running",     V.black `on` V.white)
    , ("build.unknown",     V.black `on` V.white)
    
    , ("status.normal",     V.blue `on` V.white)
    ]

data CustomEvent = VtyEvent V.Event
                 | ReposUpdate [InternalRepo]
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

