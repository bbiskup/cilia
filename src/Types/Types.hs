{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Maybe(fromMaybe)
import GHC.Generics
import Lens.Micro((^.))
import Lens.Micro.TH(makeLenses)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF
import Data.Aeson(FromJSON, ToJSON)

data Repo =
    Repo { _slug :: Maybe T.Text
         , _description :: Maybe T.Text
         , _last_build_state :: Maybe T.Text 
         , _last_build_number :: Maybe T.Text
         , _last_build_duration :: Maybe Int
         , _last_build_finished_at :: Maybe T.Text 
    }deriving(Eq, Show, Generic)

instance FromJSON Repo
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
