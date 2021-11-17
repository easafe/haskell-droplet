{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Command (Column (..), ColumnType (..), Options (..), Table (..), Typed (..), Wrapper (..))
import qualified Command as C
import Query (ColumnDefinition (..))
import Test.Hspec
import Data.Text (Text)

defaultOptions :: Options
defaultOptions =
    Options
        { input = Nothing
        , schema = Nothing
        , connectionUrl = Just "conn"
        , definitionsFolder = Nothing
        , moduleBaseName = Nothing
        }

main :: IO ()
main = hspec $ do
    describe "define" $ do
        describe "makeTables" $ do
            it "parses names into camel case" $ do
                _a
                    `shouldBe`_b
            it "parses wrapper" $ do
                _c
                    `shouldBe` _d
            it "parses type" $ do
                _e
                    `shouldBe` _f
            it "groups columns into tables" $ do
                _g
                    `shouldBe` _h