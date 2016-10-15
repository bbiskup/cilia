{-# LANGUAGE OverloadedStrings #-}
module Main where


import Brick.Main(defaultMain)
import Ui



main :: IO ()
main = do 
    let s = AppState { travisUser = "bbiskup"}
    defaultMain app s
    return ()