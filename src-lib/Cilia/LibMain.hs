{-# LANGUAGE OverloadedStrings #-}
module Cilia.LibMain(libMain) where

import Prelude
import Data.Monoid((<>))
import Control.Monad(void)
import Control.Concurrent(newChan, forkIO)
import Data.Time.Clock(getCurrentTime)
import Data.Default(def)
import Brick.Main(customMain)
import qualified Graphics.Vty as V
import Options.Applicative
import qualified GHC.Conc as GO

import Cilia.Opts( Opts(..)
                 , optsParser)
import Cilia.Ui( AppState(..)
               , app)
import qualified Cilia.CI.CI as CI
import Cilia.Config(Config, readConfig)


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

initialState :: Config -> IO Cilia.Ui.AppState
initialState config = do 
    timestamp <- getCurrentTime
    return Cilia.Ui.AppState 
        { _conf = config
        , _stLastVtyEvent = Nothing
        , _repos = []
        , _timestamp = timestamp
        , _errMsg  = Nothing
        }

doMain :: Opts -> IO ()
doMain (Opts configFileName') = do
    config <- Cilia.Config.readConfig configFileName'
    chan <- newChan
    initialState' <- initialState config
    ciThread <- forkIO $ CI.checkCIServers config chan
    GO.labelThread ciThread "CI"
    void $ customMain (V.mkVty def) chan app initialState'

libMain :: IO ()
libMain = do
    optsParser' <- optsParser
    let opts = info (helper <*> optsParser')
            (fullDesc
            <> progDesc "Run cilia"
            <> header "cilia - continuous integration monitor")
    execParser opts >>= doMain
        
        