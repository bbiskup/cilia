{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Prelude
import Data.Maybe(fromMaybe)
import GHC.Generics
import Lens.Micro((^.))
import Lens.Micro.TH(makeLenses)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF
import Data.Aeson( FromJSON(..)
                 , ToJSON
                 , withObject
                 , (.:?)
                 , (.:))

data Conf =
    Conf { _travisUser :: T.Text 
         } deriving (Eq, Show)

makeLenses ''Conf

data BuildState =
      Passed
    | Failed
    | Unknown
    deriving(Ord, Eq, Show)

instance FromJSON BuildState where
    parseJSON "failed" = return Failed
    parseJSON "passed" = return Passed
    parseJSON _ = return Unknown

data Repo =
    Repo { _slug :: Maybe T.Text
         , _description :: Maybe T.Text
         , _lastBuildState :: Maybe BuildState 
         , _lastBuildNumber :: Maybe T.Text
         , _lastBuildDuration :: Maybe Int
         , _lastBuildFinishedAt :: Maybe T.Text
         , _active :: Maybe Bool
    }deriving(Ord, Eq, Show)

instance FromJSON Repo where
    parseJSON = withObject "repo" $ \o ->
        Repo <$> o .: "slug"
             <*> o .:? "description"
             <*> o .:? "last_build_state"
             <*> o .:? "last_build_number"
             <*> o .:? "last_build_duration"
             <*> o .:? "last_build_finished_at"
             <*> o.:? "active"

makeLenses ''Repo

render :: Repo -> TL.Text
render repo =
    TF.format fmt ( getText $ repo ^. slug
                  , getText $ repo ^. description)
        where 
            getText = fromMaybe (T.pack "-")
            fmt = "{} ({})" 


data ReposResponse =
    ReposResponse { repos :: [Repo]
    } deriving (Eq, Show, Generic)

instance FromJSON ReposResponse
makeLenses ''ReposResponse
