{-# LANGUAGE OverloadedStrings #-}

module Ci (
    checkCIServers
)where

import Prelude
import Data.Maybe(fromJust)
import Control.Monad(forever)
import Control.Concurrent(Chan, writeChan, threadDelay)
import qualified Data.Text.Format as TF
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Lens.Micro((^.), (.~), (&))
import Network.Wreq

import Types(Conf, ReposResponse(..), travisUser)
import Ui(CustomEvent(..))

type Resp = Response ReposResponse


-- at least Accept header required, see https://docs.travis-ci.com/api#making-requests
opts :: Options 
opts = defaults & header "Accept" .~ ["application/vnd.travis-ci.2+json"]
                        & header "User-Agent" .~ ["cilia-useragent"]

getResp :: T.Text -> IO (Maybe ReposResponse)
getResp userName = do
    --r <- getWith opts "https://api.travis-ci.org/repos/bbiskup" 
    --let repos = r ^. responseBody . key "repos" . _Array

    let reposUrl = TL.unpack $ TF.format "https://api.travis-ci.org/repos/{}" (TF.Only userName)
    r <- asJSON =<< getWith opts reposUrl :: IO Resp
    --let (Response result) = r
    return $ Just $ r ^. responseBody

checkInterval :: Int 
checkInterval = 5 * 1000000


checkCIServers :: Conf -> Chan CustomEvent -> IO ()
checkCIServers conf chan = forever $ do
    r <- fmap fromJust $ Ci.getResp $ conf ^. travisUser
    let repos' = repos r 
    -- putStrLn $ "Repos" ++ (show repos')
    writeChan chan $ ReposUpdate repos' 
    threadDelay checkInterval
