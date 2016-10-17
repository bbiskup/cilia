{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe(fromJust)
import Control.Monad(void, forever)
import Control.Concurrent(Chan, newChan, writeChan, forkIO, threadDelay)
import Lens.Micro((^.))
import Data.Default(def)
import Brick.Main(customMain)
import qualified Graphics.Vty as V

import Types(Repo(..), repos)
import Ui(CustomEvent(..), AppState(..), app)
import qualified Ci

checkInterval :: Int 
checkInterval = 5 * 1000000

dummyRepos :: [Repo]
dummyRepos = [ 
        Repo {
               _slug = Just "myslug"
             , _description = Just "mydescription"
             , _last_build_state = Just "mylastbuildstate"
             , _last_build_number = Just "mylastbuildnumber"
             , _last_build_duration = Just 20
             , _last_build_finished_at = Just "myfinishedat"
             }
        ]

checkCIServers :: Chan CustomEvent -> IO ()
checkCIServers chan = forever $ do
    r <- fmap fromJust $ Ci.getResp "bbiskup"
    let repos' = repos r 
    -- putStrLn $ "Repos" ++ (show repos')
    writeChan chan $ ReposUpdate $ repos' 
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
