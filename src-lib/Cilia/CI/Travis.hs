{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Cilia.CI.Travis(getInternalRepos)
where

import Prelude
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
    | Failed
    | Unknown
    deriving(Ord, Eq, Show)

instance FromJSON BuildState where
    parseJSON "failed" = return Failed
    parseJSON "passed" = return Passed
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

instance IR.ToInternalRepo Repo where
    toInternalRepo travisRepo = 
      InternalRepo
          { IR._slug = travisRepo ^. slug
          , IR._description =  travisRepo ^. description
          , IR._lastBuildState = lastBuildState'
          , IR._lastBuildNumber = travisRepo ^. lastBuildNumber
          , IR._lastBuildFinishedAt = travisRepo ^. lastBuildFinishedAt
          , IR._active = travisRepo ^.  active
          }
          where lastBuildState' = undefined

{-        where lastBuildState'
          | case lastTravisBuildState of
              (Just s) = 

            where lastTravisBuildState = travisRepo ^. lastBuildState

-}

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
    return (case eitherR of
        (Right r) -> do
          let r' = repos (r ^. W.responseBody)
          Right $ fmap IR.toInternalRepo r'
        (Left s) -> Left s)