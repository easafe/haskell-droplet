{-# LANGUAGE OverloadedStrings #-}

module Constants where
import Data.Text (Text)

defaultDefinitionsFolder :: FilePath
defaultDefinitionsFolder = "Definition"

defaultModuleBaseName :: Text
defaultModuleBaseName = "Definition."

missingConnectionError :: String
missingConnectionError = "Database connection URL is required"