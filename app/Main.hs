module Main where

import Wamphf

import Data.Aeson
import Options.Applicative
import System.Exit (exitSuccess)
import System.FilePath
import System.Directory
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as B

data PathStyle = RelativePath | AbsolutePath FilePath
    deriving Show

data SearchStyle = LocalOnly | RecursiveSearch
    deriving Show

data AppConfig = AppConfig {
                        useRecursiveSearch  :: SearchStyle
                      , useRelativePaths    :: PathStyle
                      , useFilePathAsTitle  :: Bool
                      , outPath             :: Maybe FilePath
                      , inPaths             :: [FilePath]
                     } deriving Show

appConfigParse :: Parser AppConfig
appConfigParse = AppConfig
    <$> argpRecursiveSearch
    <*> (argpAbsolutePath <|> argpRelativePath)
    <*> argpFPAsTitle
    <*> (optional $ argpOutPath)
    <*> argpInPaths

argpRecursiveSearch :: Parser SearchStyle
argpRecursiveSearch = flag LocalOnly -- defaults to nonRecursiveSearch
                           RecursiveSearch
                           (short 'r' <> long "recursive" <>
                            help "TODO Search for mp3 files recursively. Defaults to input directory only search.")

argpAbsolutePath :: Parser PathStyle
argpAbsolutePath = AbsolutePath <$> option str
                         (short 'a' <> long "absolute" <>
                          help "TODO Use absolute file path style. Defaults to relative file paths.")

argpRelativePath = flag RelativePath RelativePath (internal)

argpFPAsTitle :: Parser Bool
argpFPAsTitle = flag False
                     True
                     (short 'n' <> long "FileNameAsTitle" <>
                      help "Use FileName as Title field metadata.")

argpOutPath :: Parser FilePath
argpOutPath = strOption ( short 'o' <> long "output" <> metavar "OUTPUT"
                            <> help "Output filepath for JSON Array tracklist. Defaults to STDOUT" )

argpInPaths :: Parser [FilePath]
argpInPaths = many $ argument str (metavar "INPUTDIRS..." <>
                                    help "TODO Input directory paths. Defaults to current working directory")

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
    appConfig <- execParser optsParse
    main' appConfig
    exitSuccess

--TODO Check Web Audio API limitations
allowableExtensions :: Set String
allowableExtensions = Set.fromList [".mp3"]

filterByExtensions :: Set String -> [FilePath] -> [FilePath]
filterByExtensions extensions = filter (\file -> Set.member (getExt file) extensions)
    where getExt = snd . splitExtension

main' :: AppConfig -> IO ()
--CWD STDOUT case
main' (AppConfig LocalOnly pathStyle pUseFname outputPath []) = do
    filePaths <- liftM (filterByExtensions allowableExtensions) $ listDirectory =<< getCurrentDirectory
    results <- mapM (readTrackInfo pUseFname) filePaths
    case outputPath of
        Nothing -> B.putStr . encode $ results
        (Just fp) -> encodeFile fp results
    return ()
