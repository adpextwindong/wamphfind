module Main where

import Wamphf

import Data.Aeson
import Options.Applicative
import System.Exit (exitSuccess)
import System.FilePath
import System.Directory
import Control.Monad
import Control.Lens
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as B

data PathStyle = RelativePath | AbsolutePath FilePath
    deriving Show

data SearchStyle = LocalOnly | RecursiveSearch
    deriving Show

data FilePathAsTitleStyle = FullPathTitle | StripExtTitle | DontSwapTitle
    deriving Show

data AppConfig = AppConfig {
                        useRecursiveSearch  :: SearchStyle
                      , useRelativePaths    :: PathStyle
                      , useFilePathAsTitle  :: FilePathAsTitleStyle
                      , outPath             :: Maybe FilePath
                      , inPaths             :: [FilePath]
                     } deriving Show

appConfigParse :: Parser AppConfig
appConfigParse = AppConfig
    <$> argpRecursiveSearch
    <*> (argpAbsolutePath <|> argpRelativePath)
    <*> ((argpFullpathAsTitle <|> argpExtAsTitle) <|> argpDontSwapTitle)
    <*> optional argpOutPath
    <*> argpInPaths

argpRecursiveSearch :: Parser SearchStyle
argpRecursiveSearch = flag LocalOnly -- defaults to nonRecursiveSearch
                           RecursiveSearch
                           (short 'r' <> long "recursive" <>
                            help "TODO Search for mp3 files recursively. Defaults to input directory only search.")

argpAbsolutePath :: Parser PathStyle
argpAbsolutePath = AbsolutePath <$> option str
                         (short 'a' <> long "absolute" <>
                          help "Use absolute file path style. Defaults to relative file paths.")

argpRelativePath = flag RelativePath RelativePath internal --default


argpFullpathAsTitle :: Parser FilePathAsTitleStyle
argpFullpathAsTitle = flag' FullPathTitle
                            (short 'n' <> long "FileNameAsTitle" <>
                             help "Use FileName as Title field metadata.")

argpExtAsTitle :: Parser FilePathAsTitleStyle
argpExtAsTitle = flag' StripExtTitle
                       (short 'e' <> long "StripExtAsTitle" <>
                        help "Use FileName with all extension stripped as Title field metadata.")


argpDontSwapTitle = flag DontSwapTitle DontSwapTitle internal --default

argpOutPath :: Parser FilePath
argpOutPath = strOption ( short 'o' <> long "output" <> metavar "OUTPUT_TARGET"
                            <> help "Output filepath for JSON Array tracklist. Defaults to STDOUT" )

argpInPaths :: Parser [FilePath]
argpInPaths = many $ Options.Applicative.argument str (metavar "INPUTDIRS..." <>
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

findMusicFromDirectory :: SearchStyle -> FilePath -> IO [FilePath]
findMusicFromDirectory LocalOnly       = fmap (filterByExtensions allowableExtensions) . listDirectory
findMusicFromDirectory RecursiveSearch = undefined --TODO

--Nothing redirects to STDOUT
outputEncodeToTarget :: ToJSON a => a -> Maybe FilePath -> IO ()
outputEncodeToTarget results Nothing   = B.putStr . encode $ results
outputEncodeToTarget results (Just fp) = encodeFile fp results

swapTitleWithFname :: FilePath -> TrackInfo -> TrackInfo
swapTitleWithFname fp tinfo = over (metaData . _Just . title . _Just) (const fname) tinfo
    where fname = takeFileName fp

applyBasenameToUrl :: String -> TrackInfo -> TrackInfo
applyBasenameToUrl bname tinfo = tinfo & url .~ prependedURL
    where
        properBname = if hasTrailingPathSeparator bname
                      then bname
                      else bname ++ "/"
        prependedURL = properBname ++ (tinfo ^. url)

-- Compose optional endomorphisms
endoPChain :: [(Bool, (a -> a))] -> (a -> a)
endoPChain = foldl (.) id . map snd . filter fst

endoMaybeChain :: [Maybe (a -> a)] -> (a -> a)
endoMaybeChain = foldl (.) id . catMaybes

applyCfg :: AppConfig -> TrackInfo -> FilePath -> TrackInfo
applyCfg cfg tinfo fp = flagPipeline tinfo
    where
        bnamePF = case useRelativePaths cfg of
                    RelativePath -> Nothing
                    AbsolutePath bname -> Just $ applyBasenameToUrl bname

        titlePF = case useFilePathAsTitle cfg of
                    DontSwapTitle -> Nothing
                    FullPathTitle -> Just $ swapTitleWithFname fp
                    StripExtTitle -> Just $ swapTitleWithFname $ dropExtensions fp

        --Watch out for ordering
        flagPipeline = endoMaybeChain [titlePF, bnamePF]

ts = TrackInfo "fn" $ Just $ TrackMetaData Nothing (Just "test") Nothing Nothing Nothing

main' :: AppConfig -> IO ()
main' cfg@(AppConfig LocalOnly pathStyle pUseFname outputPath []) = do
    filePaths <- findMusicFromDirectory LocalOnly =<< getCurrentDirectory
    tracks <- mapM readTrackInfo filePaths
    let results = uncurry (applyCfg cfg) <$> zip tracks filePaths
    outputEncodeToTarget results outputPath
