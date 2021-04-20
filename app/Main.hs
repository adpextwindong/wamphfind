{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib

import GHC.Generics
import Data.Aeson

import Options.Applicative
import System.Exit (exitSuccess)
import System.FilePath
import System.Directory

data TrackMetaData = TrackMetaData {
                        artist :: String
                      , title  :: String
                     } deriving (Generic, Show)

instance ToJSON TrackMetaData where
    toEncoding = genericToEncoding defaultOptions

data TrackInfo = TrackInfo {
                    trackURL :: FilePath
                --, duration :: Float
                  , trackMeta :: TrackMetaData
                 } deriving (Generic, Show)

instance ToJSON TrackInfo where
    toEncoding = genericToEncoding defaultOptions

data PathStyle = RelativePath | AbsolutePath
    deriving Show

data SearchStyle = LocalOnly | RecursiveSearch
    deriving Show

data AppConfig = AppConfig {
                        useRecursiveSearch  :: SearchStyle
                      , useRelativePaths    :: PathStyle
                      , outPath             :: Maybe FilePath
                      , inPaths             :: [FilePath]
                     } deriving Show

appConfigParse :: Parser AppConfig
appConfigParse = AppConfig
    <$> argpRecursiveSearch
    <*> argpRelativePaths
    <*> (optional $ argpOutPath)
    <*> argpInPaths

argpRecursiveSearch :: Parser SearchStyle
argpRecursiveSearch = flag LocalOnly -- defaults to nonRecursiveSearch
                           RecursiveSearch
                           (short 'r' <> long "recursive" <>
                            help "Search for mp3 files recursively. Defaults to input directory only search.")

argpRelativePaths :: Parser PathStyle
argpRelativePaths = flag RelativePath -- defaults to
                         AbsolutePath
                         (short 'a' <> long "absolute" <>
                          help "Use absolute file path style. Defaults to relative file paths.")

argpOutPath :: Parser FilePath
argpOutPath = strOption ( short 'o' <> long "output" <> metavar "OUTPUT"
                            <> help "Output filepath for JSON Array tracklist. Defaults to STDOUT" )

argpInPaths :: Parser [FilePath]
argpInPaths = many $ argument str (metavar "INPUTDIRS..." <>
                                    help "Input directory paths. Defaults to current working directory")

optsParse :: ParserInfo AppConfig
optsParse =
    info (helper <*> versionOption <*> appConfigParse)
      ( fullDesc <> header "wamphfind - WebAmp Tracklist Json Generator" <>
        progDesc "wamphfind by George Takumi Crary")
    where
       versionOption :: Parser (a -> a)
       versionOption = infoOption "0.0.1" (short 'v' <> long "version" <> help "Show version")

main :: IO ()
main = do
    print =<< execParser optsParse
    exitSuccess
