{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import GHC.Generics

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
