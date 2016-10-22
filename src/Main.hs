{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Control.Monad(void)
import Control.Concurrent(newChan, forkIO)
import Data.Time.Clock(getCurrentTime)
import Lens.Micro((^.))
import Data.Default(def)
import Brick.Main(customMain)
import qualified Graphics.Vty as V

import Types(Conf(..))
import Ui( AppState(..)
         , conf
         , app)
import qualified Ci
import qualified Config


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
staticConf :: Conf
staticConf = Conf { _travisUser = "bbiskup"}

initialState :: IO Ui.AppState
initialState = do 
    timestamp <- getCurrentTime
    return Ui.AppState 
        { _conf = staticConf
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
    initialState' <- initialState
    _ <- forkIO $ Ci.checkCIServers (initialState' ^. conf) chan
    void $ customMain (V.mkVty def) chan app initialState'
