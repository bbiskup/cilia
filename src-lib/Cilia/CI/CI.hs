{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


module Cilia.CI.CI (
    checkCIServers
)where

import Prelude
import Control.Exception as E
import Network.HTTP.Client hiding(responseBody)
import Control.Monad(forever)
import GHC.Generics
import Control.Concurrent(Chan, writeChan, threadDelay)
import qualified Data.Text.Format as TF
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BSC
import Lens.Micro((^.), (.~), (&))
import Lens.Micro.TH(makeLenses)
import Network.Wreq
import Data.Aeson( FromJSON(..))

import Cilia.Types(Repo)
import Cilia.Ui(CustomEvent(..))
import Cilia.Config(Config, travis, userName, defaultSection, refreshInterval)


data ReposResponse =
    ReposResponse { repos :: [Repo]
    } deriving (Eq, Show, Generic)

instance FromJSON ReposResponse
makeLenses ''ReposResponse

type Resp = Response ReposResponse

-- at least Accept header required, see https://docs.travis-ci.com/api#making-requests
opts :: Options 
opts = defaults & header "Accept" .~ ["application/vnd.travis-ci.2+json"]
                        & header "User-Agent" .~ ["cilia-useragent"]


handler :: HttpException -> IO (Either String Resp)
handler (StatusCodeException s _ _ ) = return $ Left $ BSC.unpack (s ^. statusMessage)
handler e = return $ Left (show e)

getResp :: T.Text -> IO (Either String ReposResponse)
getResp userName' = do
    let reposUrl = TL.unpack $ TF.format "https://api.travis-ci.org/repos/{}" (TF.Only userName')
    eitherR <- (Right <$> (asJSON =<< getWith opts reposUrl)) `E.catch` handler :: IO (Either String Resp)
    return (case eitherR of
        (Right r) -> Right $ r ^. responseBody
        (Left s) -> Left s)

checkCIServers :: Config -> Chan CustomEvent -> IO ()
checkCIServers conf chan = forever $ do
    let refreshInterval' = (conf ^. defaultSection . refreshInterval) * 1000000
    r' <- getResp $ conf ^. travis . userName
    case r' of 
        (Left s) -> do  
            writeChan chan $ NetworkError (T.pack s)
            threadDelay refreshInterval'
        (Right r) -> do
            let repos' = repos r 
            writeChan chan $ ReposUpdate repos' 
            threadDelay refreshInterval'
