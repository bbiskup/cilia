module Opts where

import Prelude
import Data.Monoid((<>))
import qualified Data.Text as T
import System.Environment(getEnv)
import qualified Path as P
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

defaultConfigFileName :: IO String
defaultConfigFileName = return "xxx"

optsParser :: IO (Parser Opts)
optsParser = do
    defaultConfigFileName' <- T.pack <$> defaultConfigFileName
    return $ Opts <$> ((T.pack <$> configFileOpt) <|> (pure . T.pack $ "xxx"))
    where 
        configFileOpt = strOption 
            (long "config-file"
            <> short 'c'
            <> metavar "FILENAME"
            <> help "Name of YAML configuration file")