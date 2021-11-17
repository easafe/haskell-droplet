{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Command.Define (define) where

import Command.Internal.Query (ColumnDefinition (..))
import qualified Command.Internal.Query as CIQ
import Command.Types (FileOutput (..), Options (..))
import Constants (
    autoType,
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
    whereKeyword, space
 )
import qualified Data.Foldable as DF
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as DHS
import Data.Hashable (Hashable)
import qualified Data.List as DL
import qualified Data.Maybe as DM
import Data.Text (Text, empty)
import qualified Data.Text as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.IO as DTI
import qualified Database.PostgreSQL.Simple as DPS
import GHC.Generics (Generic)
import qualified System.Directory as SD
import qualified Text.Casing as TC
import Prelude hiding (print)
import Debug.Trace (traceShow)

-- | A table marks the definition and the column row type
data Table = Table
    { originalName :: Text
    , camelCaseName :: Text
    , columns :: [Column]
    }
    deriving (Show, Eq)

-- | Row Type information
data Column = Column
    { originalName :: Text
    , camelCaseName :: Text
    , dataType :: ColumnType
    }
    deriving (Show, Eq)

data ColumnType = ColumnType
    { wrapper :: Wrapper
    , typed :: Typed
    }
    deriving (Show, Eq)

data Wrapper
    = Auto
    | -- | `Maybe` in the output
      Nullable
    | Default
    | -- | No wrapper for this type
      None
    deriving (Show, Eq)

data Typed
    = String
    | Int
    | Number
    | Date
    | DateTime
    | Boolean
    | -- | Outputted as a typed hole
      Unknown
    deriving (Show, Eq, Generic)

instance Hashable Typed

define :: Options -> IO ()
define Options{connectionUrl, input, schema, moduleBaseName, definitionsFolder} = do
    columns <- CIQ.fetchColumnDefinitions (DM.fromJust connectionUrl) schema input
    let m = DM.fromJust moduleBaseName
        d = DM.fromJust definitionsFolder
    SD.createDirectoryIfMissing True d
    DF.traverse_ (saveFile d . print m) $ makeTables columns
    putStrLn $ "Outputted files to " <> d

makeTables :: [ColumnDefinition] -> [Table]
makeTables columns = map toTable grouped
    where
        grouped = DL.groupBy (\c d -> table_name c == table_name d) columns

        toTable definitions =
            let original = table_name $ head definitions
             in Table
                    { originalName = original
                    , camelCaseName = toCamelCase original
                    , columns = map toColumn definitions
                    }

        toColumn def@ColumnDefinition{column_name} =
            Column
                { originalName = column_name
                , camelCaseName = toCamelCase column_name
                , dataType = toColumnType def
                }

        toColumnType ColumnDefinition{data_type, is_nullable, is_identity, column_default} =
            let wrapper
                    | is_nullable = Nullable
                    | is_identity = Auto
                    | DM.isJust column_default = Default
                    | otherwise = None
                typed
                    | data_type == "text" || DL.isPrefixOf "char" data_type = String
                    | data_type == "integer" || data_type == "smallint" || data_type == "bigint" = Int
                    | DL.isPrefixOf "numeric" data_type || data_type == "decimal" || data_type == "real" || DL.isPrefixOf "double" data_type = Number
                    | data_type == "date" = Date
                    | DL.isPrefixOf "timestamp" data_type = DateTime
                    | data_type == "boolean" = Boolean
                    | otherwise = Unknown
             in ColumnType{wrapper = wrapper, typed = typed}

        toCamelCase = DT.pack . TC.camel . DT.unpack

print :: Text -> Table -> FileOutput
print moduleBaseName Table{originalName, camelCaseName, columns} =
    FileOutput
        { name = DT.unpack titleName <> pureScriptExtension
        , contents = DT.intercalate newLine [header, rowType, table, proxies]
        }
    where
        header = disclaimer <> moduleKeyword <> moduleBaseName <> dot <> titleName <> whereKeyword <> defaultImportList <> extraImports

        rowType =
            let fieldSeparator = newLine <> ident <> comma
             in typeKeyword <> titleName <> equals <> newLine
                    <> ident
                    <> openBracket
                    <> DS.intercalate fieldSeparator (map toField columns)
                    <> newLine
                    <> ident
                    <> closeBracket
                    <> newLine

        table =
            camelCaseName <> doubleColon <> quote <> originalName <> quote <> newLine
                <> camelCaseName
                <> equals
                <> space
                <> tableType
                <> newLine

        proxies = DT.intercalate newLine $ map toProxy columns

        extraImports =
            let dataTypes = map (typed . dataType) columns
                moduleDataType = DHS.fromList [(Date, dateImport), (DateTime, dateTimeImport)]
                include running dt
                    | DL.elem dt dataTypes = running <> moduleDataType ! dt
                    | otherwise = running
             in DL.foldl include empty [Date, DateTime]

        toField Column{originalName, camelCaseName, dataType = ColumnType{wrapper, typed}} =
            let w =
                    case wrapper of
                        Auto -> autoType
                        Nullable -> nullableType
                        Default -> defaultType
                        None -> empty
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

        titleName = DT.pack . TC.pascal $ DT.unpack camelCaseName

saveFile :: FilePath -> FileOutput -> IO ()
saveFile folderName FileOutput{name, contents} = do
    let fileName = folderName <> slash <> name
    DTI.writeFile fileName contents