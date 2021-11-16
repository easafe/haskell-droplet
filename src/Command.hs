{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Command (Options(..), define) where

import qualified Data.List as DL
import qualified Data.Maybe as DM
import Data.Text (Text)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE

import qualified Database.PostgreSQL.Simple as DPS
import GHC.Generics (Generic)
import qualified Query as Q
import Query (ColumnDefinition(..))

data Options = Options
    { input :: Maybe String
    , schema :: Maybe String
    , connectionUrl :: Maybe String
    , definitionsFolder :: Maybe String
    , moduleBaseName :: Maybe Text
    }
    deriving (Show)

data Table = Table
    { originalName :: String
    , camelCaseName :: String
    , columns :: [Column]
    }

data Column = Column
    { originalName :: String
    , camelCaseName :: String
    , dataType :: ColumnType
    }

data ColumnType = ColumnType Wrapper Typed

data Wrapper
    = Auto
    | Nullable
    | Default

data Typed = String | Int | Number | Boolean

define :: Options -> IO ()
define Options{connectionUrl, input, schema} = do
    columns <- Q.fetchAllColumns (DM.fromJust connectionUrl) schema input
    tables <- makeTables columns

    pure ()

makeTables :: [ColumnDefinition] -> IO [[Column]]
makeTables columns = do
    let tables = DL.groupBy tableName columns
    pure []
  where
    tableName c d = table_name c == table_name d