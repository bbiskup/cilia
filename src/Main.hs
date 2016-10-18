{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(void)
import Control.Concurrent(newChan, forkIO)
import Lens.Micro((^.))
import Data.Default(def)
import Brick.Main(customMain)
import qualified Graphics.Vty as V

import Types( Conf(..)
            -- , travisUser
            , BuildState(Passed)
            , Repo(..))
import Ui( AppState(..)
         , conf
         , app)
import qualified Ci


dummyRepos :: [Repo]
dummyRepos = [ 
        Repo {
               _slug = Just "myslug"
             , _description = Just "mydescription"
             , _lastBuildState = Just Passed
             , _lastBuildNumber = Just "mylastbuildnumber"
             , _lastBuildDuration = Just 20
             , _lastBuildFinishedAt = Just "myfinishedat"
             , _active = Just True
             }
        ]

staticConf :: Conf
staticConf = Conf { _travisUser = "bbiskup"}

initialState :: Ui.AppState
initialState = Ui.AppState 
    { _conf = staticConf
    , _stLastVtyEvent = Nothing
    , _repos = []
    }


main :: IO ()
main = do
    chan <- newChan
    _ <- forkIO $ Ci.checkCIServers (initialState ^. conf) chan
    void $ customMain (V.mkVty def) chan app initialState
