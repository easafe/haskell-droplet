{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Command.Define (define) where

import Command.Internal.Query (ColumnDefinition (..))
import qualified Command.Internal.Query as CIQ
import Command.Types (FileOutput (..), Options (..))
import Constants (
    identityType,
    booleanType,
    closeBracket,
    comma,
    dash,
    dateImport,
    dateTimeImport,
    dateTimeType,
    dateType,
    defaultImportList,
    defaultType,
    disclaimer,
    doubleColon,
    equals,
    ident,
    intType,
    moduleKeyword,
    newLine,
    nullableType,
    numberType,
    openBracket,
    proxyType,
    pureScriptExtension,
    questionMark,
    quote,
    slash,
    stringType,
    tableType,
    dot,
    typeKeyword,
    whereKeyword,
    space
 )
import qualified Data.Foldable as DF
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as DHS
import Data.Hashable (Hashable)
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Database.PostgreSQL.Simple as DPS
import GHC.Generics (Generic)
import qualified System.Directory as SD
import qualified Text.Casing as TC
import Prelude hiding (print)

-- | A table marks the definition and the column row type
data Table = Table
    { originalName :: String
    , camelCaseName :: String
    , columns :: [Column]
    }
    deriving (Show, Eq)

-- | Row Type information
data Column = Column
    { originalName :: String
    , camelCaseName :: String
    , dataType :: ColumnType
    }
    deriving (Show, Eq)

data ColumnType = ColumnType
    { constraint :: [Constraint]
    , typed :: Typed
    }
    deriving (Show, Eq)

data Constraint = Constraint {
    name :: Maybe String,
    typed :: ConstraintType
}deriving (Show, Eq)

data ConstraintType
    = Identity
    |      Nullable  -- | `Maybe` in the output
    | Default
    |     Unique
    |      None --  | No constraint for this type
    deriving (Show, Eq)

data Typed
    = String
    | Int
    | Number
    | Date
    | DateTime
    | Boolean
    |       Unknown -- | Outputted as a typed hole
    deriving (Show, Eq, Generic)

instance Hashable Typed

-- | Outputs types for table(s)
define :: Options -> IO ()
define Options{connectionUrl, input, schema, moduleBaseName, definitionsFolder} = do
    columns <- CIQ.fetchColumnDefinitions (DM.fromJust connectionUrl) schema input
    let moduleName = DM.fromJust moduleBaseName
        folderName = DM.fromJust definitionsFolder
    SD.createDirectoryIfMissing True folderName
    DF.traverse_ (saveFile folderName . print moduleName) $ makeTables columns
    putStrLn $ "Outputted files to " <> folderName

makeTables :: [ColumnDefinition] -> [Table]
makeTables columns = map toTable grouped
    where
        grouped = DL.groupBy (\a b -> table_name a == table_name b) columns

        toTable definitions =
            let original = table_name $ head definitions
             in Table
                    { originalName = original
                    , camelCaseName = TC.camel original
                    , columns = map toColumn definitions
                    }

        toColumn def@ColumnDefinition{column_name} =
            Column
                { originalName = column_name
                , camelCaseName = TC.camel column_name
                , dataType = toColumnType def
                }

        toColumnType ColumnDefinition{data_type, is_nullable, is_identity, column_default} =
            let constraint
                    | DM.isJust column_default = Default
                    | is_nullable = Nullable
                    | is_identity = Identity
                    | otherwise = None
                typed
                    | data_type == "text" || DL.isPrefixOf "char" data_type = String
                    | data_type == "integer" || data_type == "smallint" || data_type == "bigint" = Int
                    | DL.isPrefixOf "numeric" data_type || data_type == "decimal" || data_type == "real" || DL.isPrefixOf "double" data_type = Number
                    | data_type == "date" = Date
                    | DL.isPrefixOf "timestamp" data_type = DateTime
                    | data_type == "boolean" = Boolean
                    | otherwise = Unknown
             in ColumnType{constraint = constraint, typed = typed}

print :: String -> Table -> FileOutput
print moduleBaseName Table{originalName, camelCaseName, columns} =
    FileOutput
        { name = titleName <> pureScriptExtension
        , contents = DL.intercalate newLine [header, rowType, table, proxies]
        }
    where
        header = disclaimer <> moduleKeyword <> moduleBaseName <> dot <> titleName <> whereKeyword <> defaultImportList <> extraImports

        rowType =
            let fieldSeparator = newLine <> ident <> comma
             in typeKeyword <> titleName <> equals <> newLine
                    <> ident
                    <> openBracket
                    <> DL.intercalate fieldSeparator (map toField columns)
                    <> newLine
                    <> ident
                    <> closeBracket
                    <> newLine

        table =
            camelCaseName <> doubleColon <> tableType <> space <> quote <> originalName <> quote <>  space <> titleName <> newLine
                <> camelCaseName
                <> equals
                <> space
                <> tableType
                <> newLine

        proxies = DL.intercalate newLine $ map toProxy columns

        extraImports =
            let dataTypes = map (typed . dataType) columns
                moduleDataType = DHS.fromList [(Date, dateImport), (DateTime, dateTimeImport)]
                include running dt
                    | DL.elem dt dataTypes = running <> moduleDataType ! dt
                    | otherwise = running
             in DL.foldl include empty [Date, DateTime]

        toField Column{originalName, camelCaseName, dataType = ColumnType{constraint, typed}} =
            let w =
                    case constraint of
                        Identity -> identityType
                        Nullable -> nullableType
                        Default -> defaultType
                        None -> ""
                t = case typed of
                    String -> stringType
                    Int -> intType
                    Number -> numberType
                    Date -> dateType
                    DateTime -> dateTimeType
                    Boolean -> booleanType
                    Unknown -> questionMark <> camelCaseName
             in camelCaseName <> doubleColon <> w <> t

        toProxy Column{originalName, camelCaseName} =
            let name = dash <> camelCaseName
             in name <> doubleColon <> proxyType <> space <> quote <> originalName <> quote <> newLine
                    <> name
                    <> equals
                    <> space
                    <> proxyType
                    <> newLine

        titleName = TC.pascal camelCaseName

saveFile :: FilePath -> FileOutput -> IO ()
saveFile folderName FileOutput{name, contents} = do
    let fileName = folderName <> slash <> name
    writeFile fileName contents