{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Cilia.CI.Travis(getInternalRepos)
where

import Prelude
import qualified Data.Maybe as DM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as BSC
import Lens.Micro.TH(makeLenses)
import Lens.Micro((^.), (.~), (&))
import Data.Time.Clock(UTCTime)
import Data.Time.ISO8601 as TI
import qualified Data.Text.Format as TF
import Data.Aeson( FromJSON(..)
                 , withObject
                 , (.:?)
                 , (.:))
import GHC.Generics
import qualified Network.Wreq as W
import Control.Exception as E
import Network.HTTP.Client hiding(responseBody)


import qualified Cilia.CI.InternalRepo as IR
import Cilia.CI.InternalRepo(InternalRepo(..))

data BuildState =
      Passed
    | Created
    | Started
    | Failed
    | Unknown
    deriving(Ord, Eq, Show)

instance FromJSON BuildState where
    parseJSON "failed" = return Failed
    parseJSON "passed" = return Passed
    parseJSON "created" = return Created
    parseJSON "started" = return Started
    parseJSON _ = return Unknown

type BuildNumber = T.Text 
type Slug = T.Text

data Repo =
    Repo { _slug :: Maybe Slug
         , _description :: Maybe T.Text
         , _lastBuildState :: Maybe BuildState 
         , _lastBuildNumber :: Maybe BuildNumber
         , _lastBuildDuration :: Maybe Int
         , _lastBuildFinishedAt :: Maybe UTCTime
         , _active :: Maybe Bool
    }deriving(Ord, Eq, Show)

makeLenses ''Repo

instance FromJSON Repo where
    parseJSON = withObject "repo" $ \o ->
        Repo <$> o .: "slug"
             <*> o .:? "description"
             <*> o .:? "last_build_state"
             <*> o .:? "last_build_number"
             <*> o .:? "last_build_duration"
             <*> fmap parseTimestamp (o .:? "last_build_finished_at")
             <*> o.:? "active"
       where
         parseTimestamp :: Maybe String -> Maybe UTCTime
         parseTimestamp tsStr = case tsStr of 
            Nothing -> Nothing
            (Just ts) -> TI.parseISO8601 ts

mapBuildState :: BuildState -> IR.BuildState
mapBuildState Passed = IR.Passed
mapBuildState Failed = IR.Failed
mapBuildState Created = IR.Running
mapBuildState Started = IR.Running
mapBuildState Unknown = IR.Unknown
-- TODO: get complete list of possible states

getLastBuildState :: Repo -> IR.BuildState
getLastBuildState tr =
    case tr ^. lastBuildState of
        Just s -> mapBuildState s 
        Nothing -> if DM.isNothing (tr ^. lastBuildFinishedAt) 
                   then IR.Running
                   else IR.Unknown

instance IR.ToInternalRepo Repo where
    toInternalRepo travisRepo = 
      InternalRepo
          { IR._slug = travisRepo ^. slug
          , IR._description =  travisRepo ^. description
          , IR._lastBuildState = Just $ getLastBuildState travisRepo
          , IR._lastBuildNumber = travisRepo ^. lastBuildNumber
          , IR._lastBuildDuration = travisRepo ^. lastBuildDuration
          , IR._lastBuildFinishedAt = travisRepo ^. lastBuildFinishedAt
          , IR._active = travisRepo ^.  active
          }

data ReposResponse =
    ReposResponse { repos :: [Repo]
    } deriving (Eq, Show, Generic)

instance FromJSON ReposResponse
-- makeLenses ''ReposResponse

type Resp = W.Response ReposResponse

-- at least Accept header required, see https://docs.travis-ci.com/api#making-requests
opts :: W.Options 
opts = W.defaults & W.header "Accept" .~ ["application/vnd.travis-ci.2+json"]
                        & W.header "User-Agent" .~ ["cilia-useragent"]


handler :: HttpException -> IO (Either String Resp)
handler (StatusCodeException s _ _ ) = return $ Left $ BSC.unpack (s ^. W.statusMessage)
handler e = return $ Left (show e)

getInternalRepos :: T.Text -> IO (Either String [IR.InternalRepo])
getInternalRepos userName' = do
    let reposUrl = TL.unpack $ TF.format "https://api.travis-ci.org/repos/{}" (TF.Only userName')
    eitherR <- (Right <$> (W.asJSON =<< W.getWith opts reposUrl)) `E.catch` handler :: IO (Either String Resp)
    case eitherR of
        (Right r) -> do
          let r' = repos (r ^. W.responseBody)
          return $ Right $ fmap IR.toInternalRepo r'
        (Left s) -> return $ Left s
