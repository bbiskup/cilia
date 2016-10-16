{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(void, forever)
import Control.Concurrent(newChan, forkIO, threadDelay)
import Data.Default(def)
import Brick.Main(customMain)
import qualified Graphics.Vty as V

import Ui

checkInterval :: Int 
checkInterval = 5 * 1000000

checkCIServers :: String -> IO ()
checkCIServers s = forever $ do
    putStrLn $ "Checking CI server(s)" ++ s
    threadDelay checkInterval

initialState :: Ui.AppState
initialState = Ui.AppState 
    { _travisUser = "bbiskup"
    , _stLastVtyEvent = Nothing
    }

main :: IO ()
main = do
    chan <- newChan
    ciThread <- forkIO $ checkCIServers "nix" 
    void $ customMain (V.mkVty def) chan app initialState
