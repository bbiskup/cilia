{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Prelude 
import qualified Data.Text as T
import Data.Yaml( FromJSON(..)
                , (.:)
                , decodeFileEither)
import qualified Data.Yaml as Y
import qualified GHC.Generics as G

data DefaultSection = 
    DefaultSection { refreshInterval :: Int
                  }deriving(Eq, Show, G.Generic)

instance FromJSON DefaultSection


data TravisSection = 
    TravisSection { userName :: T.Text
                  }deriving(Eq, Show, G.Generic)

instance FromJSON TravisSection

data Config = 
    Config { defaultSection :: DefaultSection
           , travis :: TravisSection
           }deriving(Eq, Show)

instance FromJSON Config where
    parseJSON (Y.Object o) = Config <$>
        o .: "default" <*>
        o .: "travis"
    parseJSON _ = fail "Expected configuration object"

readConfig :: T.Text -> IO Config
readConfig configFileName =
    either (error . show) id <$>
        decodeFileEither (T.unpack configFileName)