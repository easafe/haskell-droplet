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
    , constraint_name :: Maybe String
    , constraint_type :: Maybe String
    }
    deriving (Generic, FromRow)

-- | A flat list of columns information
fetchColumnDefinitions :: String -> Maybe String -> Maybe String -> IO [ColumnDefinition]
fetchColumnDefinitions connectionUrl (DM.fromMaybe defaultSchema -> schema) table = do
    connection <- DPS.connectPostgreSQL . DTE.encodeUtf8 $ DT.pack connectionUrl
    case table of
        Nothing ->
            DPS.query connection "SELECT table_name, column_name, data_type, cast(is_nullable as bool), cast(is_identity as bool), column_default, constraint_name, constraint_type FROM information_schema.columns AS c LEFT JOIN LATERAL (SELECT ccu.constraint_name, tc.constraint_type FROM information_schema.constraint_column_usage AS ccu JOIN information_schema.table_constraints tc ON tc.table_name = c.table_name AND tc.table_catalog = c.table_catalog AND tc.constraint_name = ccu.constraint_name WHERE ccu.column_name = c.column_name AND ccu.table_name = c.table_name AND ccu.table_schema = c.table_schema) cc ON true WHERE table_schema = ? ORDER BY table_name, column_name" $ Only schema
        Just tableName -> do
            DPS.query connection "SELECT table_name, column_name, data_type, cast(is_nullable as bool), cast(is_identity as bool), column_default, constraint_name, constraint_type FROM information_schema.columns AS c LEFT JOIN LATERAL (SELECT ccu.constraint_name, tc.constraint_type FROM information_schema.constraint_column_usage AS ccu JOIN information_schema.table_constraints tc ON tc.table_name = c.table_name AND tc.table_catalog = c.table_catalog AND tc.constraint_name = ccu.constraint_name WHERE ccu.column_name = c.column_name AND ccu.table_name = c.table_name AND ccu.table_schema = c.table_schema) cc ON true WHERE table_schema = ? AND table_name = ? ORDER BY  column_name" (schema, tableName)
