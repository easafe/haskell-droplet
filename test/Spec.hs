{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}


import Command (Options(..))
import qualified Command as C
import Test.Hspec
import qualified Data.Text.IO as DTI
import qualified Text.Casing as TC
import Constants(slash, pureScriptExtension)
import qualified System.Directory as SD
import Data.Text (Text)
import qualified Data.Maybe as DM
import qualified Data.Foldable as DF

expectedFolder :: String
expectedFolder = "test/Expected"

outputtedFolder = "test/Definition"

defaultOptions :: Options
defaultOptions =
    Options
        { input = Nothing
        , schema = Nothing
        , connectionUrl = Just "postgres://pebble:pebble@localhost/pebble"
        , definitionsFolder = Just outputtedFolder
        , moduleBaseName = Just "Test"
        }

checkFiles = DF.traverse_ $ \name -> do
    let fileName = slash <>  name <> pureScriptExtension
    generated <- DTI.readFile $ outputtedFolder <> fileName
    expected <- DTI.readFile $ expectedFolder <> fileName
    generated `shouldBe` expected

main :: IO ()
main = hspec $ do
    describe "define" $ do
        it "parses table" $ do
            C.define defaultOptions{input = Just "table_a"}
            checkFiles ["TableA"]
            SD.removeDirectoryRecursive outputtedFolder

        it "parses all tables" $ do
            C.define defaultOptions
            checkFiles ["TableA"] --"TAbleB", "Tablec"]
            SD.removeDirectoryRecursive outputtedFolder