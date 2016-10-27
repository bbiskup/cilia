{-# LANGUAGE TemplateHaskell #-}

module Cilia.CI.InternalRepo( BuildState(..)
                            , InternalRepo
                            , ToInternalRepo
                            , slug
                            , description
                            , toInternalRepo
                            , lastBuildState
                            , lastBuildNumber
                            , lastBuildDuration
                            , active
                            , lastBuildFinishedAt)
where

import Prelude
import qualified Data.Text as T
import Data.Time() -- must be imported here to provide Show instance for UTCTime
import Data.Time.Clock(UTCTime)
import Lens.Micro.TH(makeLenses)



data BuildState =
      Passed
    | Failed
    | Unknown
    deriving(Ord, Eq, Show)

makeLenses ''BuildState

type BuildNumber = T.Text 
type Slug = T.Text

data InternalRepo =
    InternalRepo { _slug :: Maybe Slug
                 , _description :: Maybe T.Text
                 , _lastBuildState :: Maybe BuildState 
                 , _lastBuildNumber :: Maybe BuildNumber
                 , _lastBuildDuration :: Maybe Int
                 , _lastBuildFinishedAt :: Maybe UTCTime
                 , _active :: Maybe Bool
    }deriving(Ord, Eq, Show)

-- conversion from vendor-specific repository type
-- to internal repository type
class ToInternalRepo a where
    toInternalRepo :: a -> InternalRepo

makeLenses ''InternalRepo