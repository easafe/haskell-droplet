{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Query where

import qualified Data.Maybe as DM
import qualified Database.PostgreSQL.Simple as DPS
import qualified Data.Text.Encoding as DTE
import qualified Data.Text as DT
import Database.PostgreSQL.Simple (FromRow, Only (..))
import GHC.Generics (Generic)
import Data.Text (Text)

-- snake case so we can use DeriveGeneric
data ColumnDefinition = ColumnDefinition
    { table_name :: Text
    , column_name :: Text
    , data_type :: String
    , is_nullable :: Bool
    , is_identity :: Bool
    }
    deriving (Generic, FromRow)

fetchAllColumns :: String -> Maybe String -> Maybe String -> IO [ColumnDefinition]
fetchAllColumns connectionUrl (DM.fromMaybe "public" -> schema) table = do
    connection <- DPS.connectPostgreSQL . DTE.encodeUtf8 $ DT.pack connectionUrl
    case table of
        Nothing ->
            DPS.query connection "SELECT table_name, column_name, data_type, is_nullable, is_identity FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema = ?" $ Only schema
        Just tableName -> do
            DPS.query connection "SELECT table_name, column_name, data_type, is_nullable, is_identity FROM INFORMATION_SCHEMA.COLUMNS WHERE table_schema = ? AND table_name = ?" (schema, tableName)