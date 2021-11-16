{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Command (Column (..), ColumnType (..), Options (..), Table (..), Typed (..), Wrapper (..))
import qualified Command as C
import Query (ColumnDefinition (..))
import Test.Hspec

defaultOptions :: Options
defaultOptions =
    Options
        { input = Nothing
        , schema = Nothing
        , connectionUrl = Just "conn"
        , definitionsFolder = Nothing
        , moduleBaseName = Nothing
        }

dummyDefinition table column =
    ColumnDefinition
        { table_name = table
        , column_name = column
        , data_type = "integer"
        , is_nullable = False
        , is_identity = False
        , column_default = Nothing
        }

dummyTable table column =
    Table
        { originalName = table
        , camelCaseName = table
        , columns =
            [ column
            ]
        }

dummyColumn name =
    Column
        { originalName = name
        , camelCaseName = name
        , dataType = ColumnType{wrapper = None, typed = Int}
        }

main :: IO ()
main = hspec $ do
    describe "define" $ do
        describe "makeTables" $ do
            it "parses names into camel case" $ do
                C.makeTables [dummyDefinition "a_a" "b_b"]
                    `shouldBe` [ Table
                                    { originalName = "a_a"
                                    , camelCaseName = "aA"
                                    , columns = [Column{originalName = "b_b", camelCaseName = "bB", dataType = ColumnType{wrapper = None, typed = Int}}]
                                    }
                               ]
            it "parses wrapper" $ do
                C.makeTables [(dummyDefinition "a" "b"){is_nullable = True}]
                    `shouldBe` [ dummyTable "a" (dummyColumn "b"){dataType = ColumnType{wrapper = Nullable, typed = Int}}
                               ]
            it "parses type" $ do
                C.makeTables [(dummyDefinition "a" "b"){data_type = "char"}]
                    `shouldBe` [ dummyTable "a" (dummyColumn "b"){dataType = ColumnType{wrapper = None, typed = String}}
                           ]
            it "groups columns into tables" $ do
                C.makeTables [dummyDefinition "table1" "a", dummyDefinition "table1" "b", dummyDefinition "table2" "c"]
                    `shouldBe` [ Table
                                    { originalName = "table1"
                                    , camelCaseName = "table1"
                                    , columns =
                                        [ Column{originalName = "a", camelCaseName = "a", dataType = ColumnType{wrapper = None, typed = Int}}
                                        , Column{originalName = "b", camelCaseName = "b", dataType = ColumnType{wrapper = None, typed = Int}}
                                        ]
                                    }
                               , Table
                                    { originalName = "table2"
                                    , camelCaseName = "table2"
                                    , columns = [Column{originalName = "c", camelCaseName = "c", dataType = ColumnType{wrapper = None, typed = Int}}]
                                    }
                               ]