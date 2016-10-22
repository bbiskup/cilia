{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Control.Monad(void)
import Control.Concurrent(newChan, forkIO)
import Data.Time.Clock(getCurrentTime)
import Data.Default(def)
import Brick.Main(customMain)
import qualified Graphics.Vty as V

import Ui( AppState(..)
         , app)
import qualified Ci
import Config(Config, readConfig)


{-dummyRepos :: [Repo]
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
-}

initialState :: Config -> IO Ui.AppState
initialState config = do 
    timestamp <- getCurrentTime
    return Ui.AppState 
        { _conf = config
        , _stLastVtyEvent = Nothing
        , _repos = []
        , _timestamp = timestamp
        , _errMsg  = Nothing
        }


main :: IO ()
main = do
    config <- Config.readConfig "cilia.yml"
    putStrLn $ "Config: \n" ++ show config
    chan <- newChan
    initialState' <- initialState config
    _ <- forkIO $ Ci.checkCIServers config chan
    void $ customMain (V.mkVty def) chan app initialState'
