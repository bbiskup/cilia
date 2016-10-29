{-# LANGUAGE OverloadedStrings #-}
module Cilia.CI.CI (
    checkCIServers
)where

import Prelude
import Control.Monad(forever)
import Control.Concurrent(Chan, writeChan, threadDelay)
import qualified Data.Text as T
import Lens.Micro((^.))

import Cilia.Ui(CustomEvent(..))
import Cilia.Config(Config, travis, defaultSection, refreshInterval)
import qualified Cilia.CI.Travis as Travis


checkCIServers :: Config -> Chan CustomEvent -> IO ()
checkCIServers conf chan = forever $ do
    let refreshInterval' = (conf ^. defaultSection . refreshInterval) * 1000000
    r' <- Travis.getInternalRepos $ conf ^. travis 
    case r' of 
        (Left s) -> do  
            writeChan chan $ NetworkError (T.pack s)
            threadDelay refreshInterval'
        (Right repos) -> do
            writeChan chan $ ReposUpdate repos
            threadDelay refreshInterval'
