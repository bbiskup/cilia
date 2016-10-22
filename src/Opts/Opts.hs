module Opts where

import Prelude
import Data.Monoid((<>))
import qualified Data.Text as T
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

optsParser :: Parser Opts
optsParser = Opts <$> (T.pack <$> (configFileOpt <|> pure "xxx"))
    where 
        configFileOpt = strOption 
            (long "config-file"
            <> short 'c'
            <> metavar "FILENAME"
            <> help "Name of YAML configuration file")