{-# LANGUAGE TemplateHaskell #-}

module Opts where

import Prelude
import Data.Monoid((<>))
import qualified Data.Text as T
import System.Environment as E
import qualified Path as P
import Path((</>))
import qualified System.Directory as D
import Options.Applicative
    ( Parser
    , strOption
    , long
    , short
    , metavar
    , help
    , (<|>))

data Opts =
    Opts { configFileName :: T.Text 
         } deriving (Show) 


defaultConfigFilePath :: IO FilePath
defaultConfigFilePath = do
    homeDir <- E.lookupEnv "HOME"
    defaultConfigFileName <- P.parseRelFile "cilia.yml"
    let noConfigFile = "<no config file>"
    case homeDir of
        Nothing -> return noConfigFile
        (Just homeDirStr) -> do
            homeDir' <- P.parseAbsDir homeDirStr
            let configInHome = P.toFilePath $ homeDir' </> defaultConfigFileName
            doesExistInHome <- D.doesFileExist configInHome 
            if doesExistInHome 
            then return configInHome
            else return noConfigFile

optsParser :: IO (Parser Opts)
optsParser = do
    defaultConfigFilePath' <- T.pack <$> defaultConfigFilePath
    return $ Opts <$> ((T.pack <$> configFileOpt) <|> pure defaultConfigFilePath')
    where 
        configFileOpt = strOption 
            (long "config-file"
            <> short 'c'
            <> metavar "FILENAME"
            <> help "Name of YAML configuration file")