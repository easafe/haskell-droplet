{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Command.Internal.Query where

import qualified Data.Maybe as DM
import qualified Database.PostgreSQL.Simple as DPS
import qualified Data.Text.Encoding as DTE
import qualified Data.Text as DT
import Database.PostgreSQL.Simple (FromRow, Only (..))
import GHC.Generics (Generic)
import Data.Text (Text)
import Constants (defaultSchema)

-- snake case so we can automatically derive
-- | Columns from INFORMATION_SCHEMA.COLUMNS that inform generated code
data ColumnDefinition = ColumnDefinition
    { table_name :: Text
    , column_name :: Text
    , data_type :: String
    , is_nullable :: Bool
    , is_identity :: Bool
    , column_default :: Maybe String
    }
    deriving (Generic, FromRow)

-- | A flat list of columns information
fetchColumnDefinitions :: String -> Maybe String -> Maybe String -> IO [ColumnDefinition]
fetchColumnDefinitions connectionUrl (DM.fromMaybe defaultSchema -> schema) table = do
    connection <- DPS.connectPostgreSQL . DTE.encodeUtf8 $ DT.pack connectionUrl
    case table of
        Nothing ->
            DPS.query connection "SELECT table_name, column_name, data_type, cast(is_nullable as bool), cast(is_identity as bool), column_default FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema = ? ORDER BY table_name, column_name" $ Only schema
        Just tableName -> do
            DPS.query connection "SELECT table_name, column_name, data_type, cast(is_nullable as bool), cast(is_identity as bool), column_default FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema = ? AND table_name = ? ORDER BY column_name" (schema, tableName)