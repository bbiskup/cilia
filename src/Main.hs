{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Data.Monoid((<>))
import qualified Data.Text as T
import Control.Monad(void)
import Control.Concurrent(newChan, forkIO)
import Data.Time.Clock(getCurrentTime)
import Data.Default(def)
import Brick.Main(customMain)
import qualified Graphics.Vty as V
import Options.Applicative

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


data Opts =
    Opts { configFileName :: T.Text 
         } deriving (Show) 

optsParser :: Parser Opts
optsParser = Opts <$>
    (T.pack <$> strOption 
            (long "config-file"
            <> short 'c'
            <> metavar "FILENAME"
            <> help "Name of YAML configuration file"))

doMain :: Opts -> IO ()
doMain (Opts configFileName') = do
    config <- Config.readConfig configFileName'
    chan <- newChan
    initialState' <- initialState config
    _ <- forkIO $ Ci.checkCIServers config chan
    void $ customMain (V.mkVty def) chan app initialState'
-- doMain _ = return ()

main :: IO ()
main = execParser opts >>= doMain
    where 
        opts = info (helper <*> optsParser)
            (fullDesc
            <> progDesc "Run cilia"
            <> header "cilia - continuous integration monitor")