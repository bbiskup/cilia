{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(forever)
import Control.Concurrent(forkIO, threadDelay)
import Brick.Main(defaultMain)
import Ui

checkInterval :: Int 
checkInterval = 5 * 1000000

checkCIServers :: IO ()
checkCIServers = forever $ do
    putStrLn "Checking CI server(s)"
    threadDelay checkInterval


main :: IO ()
main = do
    ciThread <- forkIO checkCIServers 
    let s = AppState { travisUser = "bbiskup"}
    defaultMain app s
    return ()