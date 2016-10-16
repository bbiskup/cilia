{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Maybe(fromMaybe)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF
import Data.Aeson(FromJSON, ToJSON)

data Repo =
    Repo { slug :: Maybe T.Text
         , description :: Maybe T.Text
         , last_build_state :: Maybe T.Text 
         , last_build_number :: Maybe T.Text
         , last_build_duration :: Maybe Int
         , last_build_finished_at :: Maybe T.Text 
    }deriving(Eq, Show, Generic)

render :: Repo -> TL.Text
render repo =
    TF.format fmt (getText $ slug repo, getText $ description repo)
        where 
            getText = fromMaybe (T.pack "-")
            fmt = "{} ({})" 

instance FromJSON Repo
--makeLenses ''Repo

data ReposResponse =
    ReposResponse { repos :: [Repo]
    } deriving (Eq, Show, Generic)

instance FromJSON ReposResponse
--makeLenses ''ReposResponse
