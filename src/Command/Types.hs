
{-# LANGUAGE DuplicateRecordFields #-}

module Command.Types where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Database.PostgreSQL.Simple (FromRow)

-- | Command line/environment options
data Options = Options
    { -- | Input for current command
      input :: Maybe String
    , schema :: Maybe String
    , connectionUrl :: Maybe String
    , -- | Where to output the generated code
      definitionsFolder :: Maybe String
    , moduleBaseName :: Maybe Text
    }
    deriving (Show)

data FileOutput = FileOutput
    { name :: String
    , contents :: Text
    }
