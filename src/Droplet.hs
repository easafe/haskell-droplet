module Droplet (
    Commands (..),
    Options (..),
) where

import Data.Text (Text)

data Commands
    = Define Options

data Options = Options
    { input :: Maybe String
    , connection :: Maybe String
    , definitionsFolder :: Maybe String
    , moduleBaseName :: Maybe Text
    } deriving (Show)
