module Main where

import Agda.Unused
  (UnusedOptions(..))
import Agda.Unused.Check
  (checkUnused, checkUnusedGlobal)
import Agda.Unused.Monad.Error
  (Error)
import Agda.Unused.Print
  (printError, printNothing, printUnused, printUnusedItems,
    relativizeUnused, relativizeUnusedItems)
import Config
  (CategoryFilter(..), Config(..), allCategoryLabels, defaultConfig,
    filterUnused, filterUnusedItems, findConfigFile, loadConfig,
    parseCategory)

import Control.Monad
  (unless)
import Control.Monad.Except
  (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class
  (MonadIO, liftIO)
import Data.Aeson
  (Value(..), (.=), object)
import Data.Aeson.Text
  (encodeToLazyText)
import Data.Text
  (Text)
import qualified Data.Text
  as T
import qualified Data.Text.IO
  as I
import Data.Text.Lazy
  (toStrict)
import Options.Applicative
  (InfoMod, Parser, ParserInfo, eitherReader, execParser, flag', footer,
    fullDesc, header, help, helper, hidden, info, long, many, metavar, option,
    optional, progDesc, short, some, strArgument, strOption, switch, (<|>))
import System.Directory
  (doesDirectoryExist, doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath
  ((</>), takeDirectory)
import System.Exit
  (exitFailure, exitSuccess)
import System.IO
  (hPutStrLn, stderr)

-- ## Options

data ConfigMode
  = ConfigAuto
  | ConfigFile !FilePath
  | ConfigNone
  deriving Show

data Options
  = Options
  { optionsConfig
    :: !Config
    -- ^ Project-level settings (same type as YAML config).
  , optionsJSON
    :: !Bool
    -- ^ Whether to format output as JSON.
  , optionsConfigMode
    :: !ConfigMode
    -- ^ How to find the config file.
  } deriving Show

optionsParser
  :: Parser Options
optionsParser
  = Options
  <$> configParser
  <*> (switch
    $ short 'j'
    <> long "json"
    <> help "Format output as JSON")
  <*> (ConfigFile <$> strOption
    (long "config"
    <> metavar "FILE"
    <> help "Use this config file instead of auto-discovery")
    <|> ConfigNone <$ flag' () (long "no-config"
    <> help "Don't load any config file")
    <|> pure ConfigAuto)

configParser
  :: Parser Config
configParser
  = Config
  <$> optional (strArgument
    $ metavar "FILE")
  <*> (Just True <$ flag' () (short 'g' <> long "global"
    <> help "Treat FILE as the project's complete public interface")
    <|> Just False <$ flag' () (long "local"
    <> help "Only report private unused code (default)")
    <|> pure Nothing)
  <*> (Just . Only <$> some (option categoryReader
    $ long "only"
    <> metavar "CATEGORY"
    <> help "Only report these categories (repeatable)")
    <|> Just . AllBut <$> some (option categoryReader
    $ long "all-but"
    <> metavar "CATEGORY"
    <> help "Report all categories but these (repeatable)")
    <|> Just (AllBut []) <$ flag' () (long "all"
    <> help "Report all categories (override config filter)")
    <|> pure Nothing)
  <*> many (strOption
    $ short 'i'
    <> long "include-path"
    <> metavar "DIR"
    <> help "Look for imports in DIR"
    <> hidden)
  <*> many (strOption
    $ short 'l'
    <> long "library"
    <> metavar "LIB"
    <> help "Use library LIB"
    <> hidden)
  <*> optional (strOption
    $ long "library-file"
    <> metavar "FILE"
    <> help "Use FILE instead of the standard libraries file"
    <> hidden)
  <*> (Just False <$ flag' () (long "no-libraries"
    <> help "Don't use any library files"
    <> hidden)
    <|> pure Nothing)
  <*> (Just False <$ flag' () (long "no-default-libraries"
    <> help "Don't use default libraries"
    <> hidden)
    <|> pure Nothing)
  where
    categoryReader = eitherReader parseCategory

optionsInfo
  :: InfoMod a
optionsInfo
  = fullDesc
  <> progDesc "Check for unused code in FILE (or use 'file' from config)"
  <> header "agda-unused - check for unused code in an Agda project"
  <> footer ("Categories: " ++ allCategoryLabels)

options
  :: ParserInfo Options
options
  = info (helper <*> optionsParser) optionsInfo

-- ## Config resolution

-- | Find and load the YAML config file, if any.
-- Resolves configFile relative to the config file's directory.
resolveConfig :: ConfigMode -> FilePath -> IO (Maybe Config)
resolveConfig ConfigNone _
  = pure Nothing
resolveConfig ConfigAuto searchFrom
  = findConfigFile searchFrom >>= loadConfigPath
resolveConfig (ConfigFile p) _ = do
  exists <- doesFileExist p
  if exists
    then loadConfigPath (Just p)
    else hPutStrLn stderr ("Error: Config file not found: " ++ p)
      >> exitFailure

loadConfigPath :: Maybe FilePath -> IO (Maybe Config)
loadConfigPath Nothing
  = pure Nothing
loadConfigPath (Just path) = do
  result <- loadConfig path
  case result of
    Left err -> do
      hPutStrLn stderr ("Error loading config: " ++ err)
      exitFailure
    Right cfg -> do
      let dir = takeDirectory path
          resolved = cfg { configFile = (dir </>) <$> configFile cfg }
      pure (Just resolved)

-- ## Merge and validate

data OptionsError where

  ErrorFile
    :: FilePath
    -> OptionsError

  ErrorDirectory
    :: FilePath
    -> OptionsError

  deriving Show

printOptionsError
  :: OptionsError
  -> Text
printOptionsError (ErrorFile p)
  = "Error: File not found " <> parens (T.pack p) <> "."
printOptionsError (ErrorDirectory p)
  = "Error: Directory not found " <> parens (T.pack p) <> "."

parens
  :: Text
  -> Text
parens t
  = "(" <> t <> ")"

validateFile
  :: MonadError OptionsError m
  => MonadIO m
  => FilePath
  -> m FilePath
validateFile p = do
  exists
    <- liftIO (doesFileExist p)
  _
    <- unless exists (throwError (ErrorFile p))
  filePath
    <- liftIO (makeAbsolute p)
  pure filePath

validateDirectory
  :: MonadError OptionsError m
  => MonadIO m
  => FilePath
  -> m FilePath
validateDirectory p = do
  exists
    <- liftIO (doesDirectoryExist p)
  _
    <- unless exists (throwError (ErrorDirectory p))
  filePath
    <- liftIO (makeAbsolute p)
  pure filePath

-- | Merge CLI config with YAML config, validate paths, and produce
-- everything needed by the check functions.
mergeValidateOpts
  :: Config
  -- ^ CLI config.
  -> Maybe Config
  -- ^ YAML config (with configFile already resolved), if any.
  -> IO (FilePath, Bool, Maybe CategoryFilter, UnusedOptions)
mergeValidateOpts cli mYaml
  = runExceptT (mergeValidateOpts' cli mYaml)
  >>= mergeValidateEither

mergeValidateEither
  :: Either OptionsError (FilePath, Bool, Maybe CategoryFilter, UnusedOptions)
  -> IO (FilePath, Bool, Maybe CategoryFilter, UnusedOptions)
mergeValidateEither (Left e)
  = I.hPutStrLn stderr (printOptionsError e) >> exitFailure
mergeValidateEither (Right result)
  = pure result

mergeValidateOpts'
  :: MonadError OptionsError m
  => MonadIO m
  => Config
  -> Maybe Config
  -> m (FilePath, Bool, Maybe CategoryFilter, UnusedOptions)
mergeValidateOpts' cli mYaml = do
  let yaml = maybe defaultConfig id mYaml

  -- Merge: file
  filePath <- case configFile cli <|> configFile yaml of
    Just f  -> validateFile f
    Nothing -> liftIO
      $ hPutStrLn stderr "Error: No FILE argument and no 'file' in config."
      >> exitFailure

  -- Merge: other fields
  let globalMode
        = maybe False id (configGlobal cli <|> configGlobal yaml)
      filt
        = configFilter cli <|> configFilter yaml
      include
        = replaceIfNonEmpty (configInclude cli) (configInclude yaml)
      libraries
        = replaceIfNonEmpty (configLibraries cli) (configLibraries yaml)
      libraryFile
        = configLibraryFile cli <|> configLibraryFile yaml
      useLibraries
        = maybe True id (configUseLibraries cli <|> configUseLibraries yaml)
      useDefaultLibraries
        = maybe True id (configUseDefaultLibraries cli <|> configUseDefaultLibraries yaml)

  includePaths
    <- traverse validateDirectory include
  libraryPath
    <- traverse validateFile libraryFile

  pure
    ( filePath
    , globalMode
    , filt
    , UnusedOptions
      { unusedOptionsInclude
        = includePaths
      , unusedOptionsLibraries
        = libraries
      , unusedOptionsLibrariesFile
        = libraryPath
      , unusedOptionsUseLibraries
        = useLibraries
      , unusedOptionsUseDefaultLibraries
        = useDefaultLibraries
      }
    )

replaceIfNonEmpty :: [a] -> [a] -> [a]
replaceIfNonEmpty [] ys = ys
replaceIfNonEmpty xs _  = xs

-- ## Check

check
  :: Options
  -> IO ()
check opts = do
  cwd
    <- getCurrentDirectory
  let searchDir = case configFile (optionsConfig opts) of
        Just f  -> takeDirectory f
        Nothing -> cwd
  mYaml
    <- resolveConfig (optionsConfigMode opts) searchDir
  (filePath, globalMode, filt, unusedOpts)
    <- mergeValidateOpts (optionsConfig opts) mYaml
  _
    <- checkWith cwd filt unusedOpts filePath globalMode (optionsJSON opts)
  pure ()

checkWith
  :: FilePath
  -- ^ Current working directory for relative paths.
  -> Maybe CategoryFilter
  -- ^ Category filter for results.
  -> UnusedOptions
  -- ^ Options to use.
  -> FilePath
  -- ^ Absolute path of the file to check.
  -> Bool
  -- ^ Whether to check project globally.
  -> Bool
  -- ^ Whether to format output as JSON.
  -> IO ()
checkWith cwd filt opts p False json
  = checkUnused opts p
  >>= printResult json (printUnusedItems . filterUnusedItems filt . rel)
  where rel = if json then id else relativizeUnusedItems cwd
checkWith cwd filt opts p True json
  = checkUnusedGlobal opts p
  >>= printResult json (printUnused . filterUnused filt . rel)
  where rel = if json then id else relativizeUnused cwd

-- ## Print

printResult
  :: Bool
  -- ^ Whether to output JSON.
  -> (a -> Maybe Text)
  -> Either Error a
  -> IO ()
printResult False _ (Left e)
  = I.hPutStrLn stderr (printError e) >> exitFailure
printResult False p (Right x)
  = I.putStrLn (maybe printNothing id (p x)) >> exitSuccess
printResult True p x
  = I.putStrLn (toStrict (encodeToLazyText (printResultJSON p x)))

printResultJSON
  :: (a -> Maybe Text)
  -> Either Error a
  -> Value
printResultJSON _ (Left e)
  = encodeMessage "error" (printError e)
printResultJSON p (Right u)
  = maybe (encodeMessage "none" printNothing) (encodeMessage "unused") (p u)

encodeMessage
  :: Text
  -- ^ Type of message.
  -> Text
  -- ^ Contents of message.
  -> Value
encodeMessage t m
  = object
  [ "type"
    .= t
  , "message"
    .= m
  ]

-- ## Main

main
  :: IO ()
main
  = execParser options
  >>= check
