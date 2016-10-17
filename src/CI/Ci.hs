{-# LANGUAGE OverloadedStrings #-}

module Ci where

import qualified Data.Text.Format as TF
import qualified Data.List as L
--import Lens.Micro.TH(makeLenses)
import Data.Maybe(fromJust, fromMaybe)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as DTI
import Data.Aeson(FromJSON, Value(..))
import Data.Aeson.Lens(key, _String, _Array)
import Lens.Micro((^.), (^?), (.~), (&), (^..))
import Network.Wreq

import Types(ReposResponse(..), render)

type Resp = Response (ReposResponse)


-- at least Accept header required, see https://docs.travis-ci.com/api#making-requests
opts :: Options 
opts = defaults & header "Accept" .~ ["application/vnd.travis-ci.2+json"]
                        & header "User-Agent" .~ ["cilia-useragent"]

getResp :: T.Text -> IO (Maybe ReposResponse)
getResp userName = do
    --r <- getWith opts "https://api.travis-ci.org/repos/bbiskup" 
    --let repos = r ^. responseBody . key "repos" . _Array

    let reposUrl = TF.format "https://api.travis-ci.org/repos/{}" (TF.Only userName)
    r <- asJSON =<< getWith opts "https://api.travis-ci.org/repos/bbiskup" :: IO Resp
    --let (Response result) = r
    return $ Just $ r ^. responseBody

main' :: IO ()
main' = do
    r <- fmap fromJust $ getResp "bbiskup"
    let repos' = repos $ r
        reposTxt = L.sort . fmap render $ repos'
    --print $ "Repo 0: " ++ (TL.unpack $ (render $ repos' !! 0))
    mapM_ print reposTxt
    return ()
