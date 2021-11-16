{-# LANGUAGE OverloadedStrings #-}

module Constants where
import Data.Text (Text)

defaultDefinitionsFolder :: FilePath
defaultDefinitionsFolder = "Definition"

defaultModuleBaseName :: Text
defaultModuleBaseName = "Definition."

missingConnectionError :: String
missingConnectionError = "Database connection URL is required"

defaultSchema :: String
defaultSchema = "public"

moduleWhere :: Text
moduleWhere = "where\n\n"

defaultImportList :: Text
defaultImportList = "import Droplet.Language\nimport Type.Proxy (Proxy(..))\n\n"

dateImport :: Text
dateImport = "import Data.Date (Date)"

dateTimeImport :: Text
dateTimeImport = "import Data.DateTime (DateTime)"

newLine :: Text
newLine = "\n"

disclaimer :: Text
disclaimer = "-- AUTO GENERATED CODE\n\n"