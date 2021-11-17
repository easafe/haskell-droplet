{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}


import Command (Options(..))
import qualified Command as C
import Test.Hspec
import qualified Data.Text.IO as DTI
import qualified Text.Casing as TC
import Constants(slash, pureScriptExtension)
import Data.Text (Text)
import qualified Data.Maybe as DM

expectedFolder :: String
expectedFolder = "test/Expected"

defaultOptions :: Options
defaultOptions =
    Options
        { input = Nothing
        , schema = Nothing
        , connectionUrl = Just "postgres://pebble@localhost/pebble"
        , definitionsFolder = Just "test/Definition"
        , moduleBaseName = Just "Test"
        }

runDefine :: Options -> String -> IO ()
runDefine options@Options{input, definitionsFolder} compare  = do
    C.define options
    generated <- DTI.readFile $ DM.fromJust definitionsFolder <> slash <> TC.pascal (DM.fromJust input) <> pureScriptExtension
    expected <- DTI.readFile $ expectedFolder <> "/" <> compare
    generated `shouldBe` expected

main :: IO ()
main = hspec $ do
    describe "define" $ do
        it "parses names into camel case" $
            runDefine defaultOptions{input = Just "table_a"} "CamelCase.purs"
        -- it "parses wrapper" $ do
        --     _c
        --         `shouldBe` _d
        -- it "parses type" $ do
        --     _e
        --         `shouldBe` _f
        -- it "groups columns into tables" $ do
        --     _g
        --         `shouldBe` _h