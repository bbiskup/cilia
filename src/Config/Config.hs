{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import Prelude 
import Data.Maybe(fromMaybe)
import qualified Data.Text as T
import Data.Yaml( FromJSON(..)
                , (.:)
                , (.:?)
                , decodeFileEither)
import qualified Data.Yaml as Y
import Lens.Micro.TH(makeLenses)

data DefaultSection = 
    DefaultSection { _refreshInterval :: Int
                   }deriving(Eq, Show)

instance FromJSON DefaultSection where
    parseJSON (Y.Object o) = DefaultSection <$>
        fmap (fromMaybe 10) (o .:? "refreshInterval")
    parseJSON _ = error "Expected travis section"

makeLenses ''DefaultSection


data TravisSection = 
    TravisSection { _userName :: T.Text
                  }deriving(Eq, Show)

instance FromJSON TravisSection where
    parseJSON (Y.Object o) = TravisSection <$>
        o .: "userName"
    parseJSON _ = error "Expected travis section"

makeLenses ''TravisSection

data Config = 
    Config { _defaultSection :: DefaultSection
           , _travis :: TravisSection
           }deriving(Eq, Show)

instance FromJSON Config where
    parseJSON (Y.Object o) = Config <$>
        o .: "default" <*>
        o .: "travis"
    parseJSON _ = fail "Expected configuration object"

makeLenses ''Config

readConfig :: T.Text -> IO Config
readConfig configFileName =
    either (error . show) id <$>
        decodeFileEither (T.unpack configFileName)