{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(void, forever)
import Control.Concurrent(Chan, newChan, writeChan, forkIO, threadDelay)
import Data.Default(def)
import Brick.Main(customMain)
import qualified Graphics.Vty as V

import Types(Repo(..), )
import Ui(CustomEvent(..), AppState(..), app)

checkInterval :: Int 
checkInterval = 5 * 1000000

checkCIServers :: Chan CustomEvent -> IO ()
checkCIServers chan = forever $ do
    writeChan chan $ ReposUpdate [ 
        Repo {
               _slug = Just "myslug"
             , _description = Just "mydescription"
             , _last_build_state = Just "mylastbuildstate"
             , _last_build_number = Just "mylastbuildnumber"
             , _last_build_duration = Just 20
             , _last_build_finished_at = Just "myfinishedat"
             }
        ]
    threadDelay checkInterval

initialState :: Ui.AppState
initialState = Ui.AppState 
    { _travisUser = "bbiskup"
    , _stLastVtyEvent = Nothing
    , _repos = []
    }

main :: IO ()
main = do
    chan <- newChan
    ciThread <- forkIO $ checkCIServers chan
    void $ customMain (V.mkVty def) chan app initialState
