{-# LANGUAGE OverloadedStrings #-}

module Constants where
import Data.Text (Text)

defaultDefinitionsFolder :: FilePath
defaultDefinitionsFolder = "Definition"

defaultModuleBaseName :: Text
defaultModuleBaseName = "Definition"

dot :: Text
dot = "."

missingConnectionError :: String
missingConnectionError = "Database connection URL is required"

defaultSchema :: String
defaultSchema = "public"

moduleKeyword :: Text
moduleKeyword = "module "

whereKeyword :: Text
whereKeyword = " where\n\n"

defaultImportList :: Text
defaultImportList = "import Droplet.Language\nimport Type.Proxy (Proxy(..))\n"

dateImport :: Text
dateImport = "import Data.Date (Date)\n"

dateTimeImport :: Text
dateTimeImport = "import Data.DateTime (DateTime)\n"

newLine :: Text
newLine = "\n"

disclaimer :: Text
disclaimer = "-- AUTO GENERATED CODE\n\n"

typeKeyword :: Text
typeKeyword = "type "

equals :: Text
equals = " ="

openBracket :: Text
openBracket = "( "

doubleColon :: Text
doubleColon = " :: "

comma :: Text
comma = ", "

space :: Text
space = " "

closeBracket :: Text
closeBracket = ")"

ident :: Text
ident = "    "

autoType :: Text
autoType = "Auto "

nullableType :: Text
nullableType = "Maybe "

defaultType :: Text
defaultType = "Default "

questionMark :: Text
questionMark = "?"

stringType :: Text
stringType = "String"

intType :: Text
intType = "Int"

numberType :: Text
numberType = "Number"

dateType :: Text
dateType = "Date"

dateTimeType :: Text
dateTimeType = "DateTime"

booleanType :: Text
booleanType = "Boolean"

tableType :: Text
tableType = "Table"

quote :: Text
quote = "\""

dash :: Text
dash = "_"

proxyType :: Text
proxyType = "Proxy"

pureScriptExtension :: String
pureScriptExtension = ".purs"

slash :: String
slash = "/"