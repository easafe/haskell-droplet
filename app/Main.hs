{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Command as C
import Constants (defaultDefinitionsFolder, defaultModuleBaseName, missingConnectionError)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (isSpace)
import qualified Data.Maybe as DM
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import Command (Options (..))
import qualified LoadEnv as LE
import Options.Applicative (Parser, ParserInfo, (<**>))
import qualified Options.Applicative as OA
import qualified System.Environment as SE

-- | Commands available
--
-- This datatype is used only here, for parsing
data Commands
    = Define Options

commandParser :: Parser Commands
commandParser =
    OA.hsubparser
        ( OA.command
            "define"
            ( OA.info
                (Define <$> optionsParser)
                (OA.progDesc "Export type definitions for table(s)")
            )
        )

optionsParser :: Parser Options
optionsParser =
    Options
        <$> OA.optional (OA.argument OA.str (OA.metavar "INPUT"))
         <*> OA.optional
            ( OA.strOption $
                OA.long "schema"
                    <> OA.short 's'
                    <> OA.help "Database schema"
                    <> OA.metavar "STRING"
            )
        <*> OA.optional
            ( OA.strOption $
                OA.long "connection-url"
                    <> OA.short 'c'
                    <> OA.help "Database connection URL"
                    <> OA.metavar "STRING"
            )
        <*> OA.optional
            ( OA.strOption $
                OA.long "definitions-folder"
                    <> OA.short 'f'
                    <> OA.help "Folder to export type definitions"
                    <> OA.metavar "STRING"
            )
        <*> OA.optional
            ( OA.strOption $
                OA.long "module-base-name"
                    <> OA.short 'm'
                    <> OA.help "Output module base name"
                    <> OA.metavar "STRING"
            )

getEnvironment :: IO Options
getEnvironment =
    Options Nothing
        <$> SE.lookupEnv "schema"
        <*> SE.lookupEnv "connectionUrl"
        <*> SE.lookupEnv "definitionsFolder"
        <*> ((DT.pack <$>) <$> SE.lookupEnv "moduleBaseName")

info :: ParserInfo Commands
info =
    OA.info
        (commandParser <**> OA.helper)
        ( OA.fullDesc
            <> OA.header "droplet - run migrations or generate definitions"
            <> OA.progDesc ""
        )

main :: IO ()
main = do
    LE.loadEnv
    environment <- getEnvironment
    command <- OA.execParser info
    case command of
        Define options -> do
            mangled <- validateOptions environment options
            C.define mangled
  where
    --command line arguments take precedence over environment variables
    -- only connectionUrl is mandatory
    validateOptions environment options = do
        let mangled =
                Options
                    { input = input options
                    , schema = schema options <|> schema environment
                    , connectionUrl = connectionUrl options <|> connectionUrl environment
                    , definitionsFolder = definitionsFolder options <|> definitionsFolder environment <|> Just defaultDefinitionsFolder
                    , moduleBaseName = moduleBaseName options <|> moduleBaseName environment <|> Just defaultModuleBaseName
                    }
        when (DM.isNothing (connectionUrl mangled) || (trim <$> connectionUrl mangled) == Just "") $ error missingConnectionError
        pure mangled

    trim = let f = reverse . dropWhile isSpace in f . f
