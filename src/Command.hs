{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Command (Options (..), define, makeTables, print) where

import Constants (
    dateImport,
    dateTimeImport,
    defaultImportList,
    disclaimer,
    moduleWhere,
    newLine,
 )
import qualified Data.Foldable as DF
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as DHS
import Data.Hashable (Hashable)
import qualified Data.List as DL
import qualified Data.Maybe as DM
import Data.Text (Text, empty)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Database.PostgreSQL.Simple as DPS
import GHC.Generics (Generic)
import Query (ColumnDefinition (..))
import qualified Query as Q
import qualified Text.Casing as TC
import Prelude hiding (print)

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

-- | A table marks the definition and the column row type
data Table = Table
    { originalName :: Text
    , columns :: [Column]
    }

-- | Row Type information
data Column = Column
    { originalName :: Text
    , dataType :: ColumnType
    }

data ColumnType = ColumnType
    { wrapper :: Wrapper
    , typed :: Typed
    }

data Wrapper
    = Auto
    | -- | `Maybe` in the output
      Nullable
    | Default
    | -- | No wrapper for this type
      None

data Typed
    = String
    | Int
    | Number
    | Date
    | DateTime
    | Boolean
    | -- | Outputted as a typed hole
      Unknown
    deriving (Eq, Generic)

instance Hashable Typed

define :: Options -> IO ()
define Options{connectionUrl, input, schema, moduleBaseName, definitionsFolder} = do
    columns <- Q.fetchColumnDefinitions (DM.fromJust connectionUrl) schema input
    let m = DM.fromJust moduleBaseName
        d = DM.fromJust definitionsFolder
    DF.traverse_ (saveFile d . print m) $ makeTables columns

makeTables :: [ColumnDefinition] -> [Table]
makeTables columns = map toTable grouped
  where
    grouped = DL.groupBy (\c d -> table_name c == table_name d) columns

    toTable definitions =
        let original = table_name $ head definitions
         in Table
                { originalName = original
                , columns = map toColumn definitions
                }

    toColumn def@ColumnDefinition{column_name} =
        Column
            { originalName = column_name
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

print :: Text -> Table -> Text
print moduleBaseName Table{originalName, columns} = header
  where
    header = disclaimer <> moduleBaseName <> DT.toTitle (toCamelCase originalName) <> moduleWhere <> defaultImportList <> extraImports <> newLine

    extraImports =
        let dataTypes = map (typed . dataType) columns
            moduleDataType = DHS.fromList [(Date, dateImport), (DateTime, dateTimeImport)]
            include running dt
                | DL.elem dt dataTypes = running <> moduleDataType ! dt
                | otherwise = running
         in DL.foldl include empty [Date, DateTime]

    toCamelCase = DT.pack . TC.camel . DT.unpack

saveFile :: FilePath -> Text -> IO ()
saveFile folderName fileContent = pure () -- do
-- let fileName = folderName <> "/" <> DT.unpack moduleBaseName <> ".hs"
-- writeFile fileName fileContent