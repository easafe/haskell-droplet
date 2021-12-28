module Constants where

defaultDefinitionsFolder :: FilePath
defaultDefinitionsFolder = "Definition"

defaultModuleBaseName :: String
defaultModuleBaseName = "Definition"

dot :: String
dot = "."

missingConnectionError :: String
missingConnectionError = "Database connection URL is required"

defaultSchema :: String
defaultSchema = "public"

moduleKeyword :: String
moduleKeyword = "module "

whereKeyword :: String
whereKeyword = " where\n\n"

defaultImportList :: String
defaultImportList = "import Droplet.Language\nimport Type.Proxy (Proxy(..))\n"

dateImport :: String
dateImport = "import Data.Date (Date)\n"

dateTimeImport :: String
dateTimeImport = "import Data.DateTime (DateTime)\n"

newLine :: String
newLine = "\n"

disclaimer :: String
disclaimer = "-- AUTO GENERATED CODE\n\n"

typeKeyword :: String
typeKeyword = "type "

equals :: String
equals = " ="

openBracket :: String
openBracket = "( "

doubleColon :: String
doubleColon = " :: "

comma :: String
comma = ", "

space :: String
space = " "

closeBracket :: String
closeBracket = ")"

ident :: String
ident = "    "

identityType :: String
identityType = "Identity "

nullableType :: String
nullableType = "Maybe "

defaultType :: String
defaultType = "Default "

questionMark :: String
questionMark = "?"

stringType :: String
stringType = "String"

intType :: String
intType = "Int"

numberType :: String
numberType = "Number"

dateType :: String
dateType = "Date"

dateTimeType :: String
dateTimeType = "DateTime"

booleanType :: String
booleanType = "Boolean"

tableType :: String
tableType = "Table"

quote :: String
quote = "\""

dash :: String
dash = "_"

proxyType :: String
proxyType = "Proxy"

pureScriptExtension :: String
pureScriptExtension = ".purs"

slash :: String
slash = "/"