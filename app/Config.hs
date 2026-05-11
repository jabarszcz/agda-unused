{- |
Module: Config

Per-project configuration file support for agda-unused.

Discovers and parses @.agda-unused.yaml@ files by walking up from
the target file's directory toward the filesystem root.
-}
module Config
  ( Category(..)
  , CategoryFilter(..)
  , Config(..)
  , categoryLabel
  , categoryOf
  , defaultConfig
  , findConfigFile
  , loadConfig
  , allCategoryLabels
  , filterUnusedItems
  , filterUnused
  , parseCategory
  ) where

import Agda.Unused
  (Unused(..), UnusedItems(..))
import Agda.Unused.Types.Range
  (RangeInfo(..), RangeType(..), parseRangeType, rangeTypeLabel,
    allRangeTypes)

import Data.Aeson
  (FromJSON(..), (.:?), (.!=), withObject)
import qualified Data.ByteString
  as BS
import Data.List
  (intercalate)
import Data.Text
  (Text)
import qualified Data.Text
  as T
import Data.Yaml
  (decodeEither')
import System.Directory
  (doesFileExist)
import System.FilePath
  ((</>), takeDirectory)

-- ## Types

-- | A report category, including the special "mutual" category which
-- is not a 'RangeType'.
data Category
  = Category !RangeType
  | CategoryMutual
  deriving (Eq, Ord, Show)

-- | Category filter: either an inclusion list or exclusion list.
data CategoryFilter
  = Only [Category]
  | AllBut [Category]
  deriving Show

-- | Per-project configuration.
data Config
  = Config
  { configFile
    :: !(Maybe FilePath)
    -- ^ Default file to check (used when no FILE argument given).
  , configGlobal
    :: !(Maybe Bool)
    -- ^ Whether to check in global mode. 'Nothing' means unspecified.
  , configFilter
    :: !(Maybe CategoryFilter)
    -- ^ Category filter. 'Nothing' means unspecified (report all).
  , configInclude
    :: ![FilePath]
    -- ^ Include paths for Agda.
  , configLibraries
    :: ![Text]
    -- ^ Libraries for Agda.
  , configLibraryFile
    :: !(Maybe FilePath)
    -- ^ Alternate libraries file for Agda.
  , configUseLibraries
    :: !(Maybe Bool)
    -- ^ Whether to use library files. 'Nothing' means unspecified.
  , configUseDefaultLibraries
    :: !(Maybe Bool)
    -- ^ Whether to use default libraries. 'Nothing' means unspecified.
  } deriving Show

-- | Default configuration: everything unspecified.
defaultConfig :: Config
defaultConfig
  = Config
  { configFile = Nothing
  , configGlobal = Nothing
  , configFilter = Nothing
  , configInclude = []
  , configLibraries = []
  , configLibraryFile = Nothing
  , configUseLibraries = Nothing
  , configUseDefaultLibraries = Nothing
  }

-- ## Category

-- | Parse a single category label.
parseCategory :: String -> Either String Category
parseCategory "mutual"
  = Right CategoryMutual
parseCategory s
  = case parseRangeType s of
    Just rt -> Right (Category rt)
    Nothing -> Left ("Unknown category: " ++ s
      ++ ". Valid: " ++ allCategoryLabels)

-- | The label for a category.
categoryLabel :: Category -> String
categoryLabel CategoryMutual = "mutual"
categoryLabel (Category rt) = rangeTypeLabel rt

-- | Comma-separated list of all valid category labels, for help text.
allCategoryLabels :: String
allCategoryLabels
  = intercalate ", " (map rangeTypeLabel allRangeTypes ++ ["mutual"])

-- | Determine the category of a range info.
categoryOf :: RangeInfo -> Category
categoryOf (RangeNamed rt _) = Category rt
categoryOf RangeMutual = CategoryMutual

-- ## YAML Parsing

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    rootFile <- o .:? "file"
    globalMode <- o .:? "global"
    onlyLabels <- o .:? "only" .!= ([] :: [Text])
    allButLabels <- o .:? "all-but" .!= ([] :: [Text])
    if not (null onlyLabels) && not (null allButLabels)
      then fail "Config error: 'only' and 'all-but' are mutually exclusive."
      else pure ()
    filt <- if not (null onlyLabels)
      then case traverse (parseCategory . T.unpack) onlyLabels of
        Left err -> fail err
        Right cs -> pure (Just (Only cs))
      else if not (null allButLabels)
      then case traverse (parseCategory . T.unpack) allButLabels of
        Left err -> fail err
        Right cs -> pure (Just (AllBut cs))
      else pure Nothing
    includePaths <- o .:? "include" .!= ([] :: [Text])
    libraries <- o .:? "libraries" .!= ([] :: [Text])
    libraryFile <- o .:? "library-file"
    useLibraries <- o .:? "use-libraries"
    useDefaultLibraries <- o .:? "use-default-libraries"
    pure Config
      { configFile = T.unpack <$> rootFile
      , configGlobal = globalMode
      , configFilter = filt
      , configInclude = map T.unpack includePaths
      , configLibraries = libraries
      , configLibraryFile = T.unpack <$> libraryFile
      , configUseLibraries = useLibraries
      , configUseDefaultLibraries = useDefaultLibraries
      }

-- ## Discovery

configFileName :: String
configFileName = ".agda-unused.yaml"

-- | Walk up from the given directory looking for
-- @.agda-unused.yaml@. Returns the first one found, or 'Nothing'.
findConfigFile :: FilePath -> IO (Maybe FilePath)
findConfigFile dir = do
  let candidate = dir </> configFileName
  exists <- doesFileExist candidate
  if exists
    then pure (Just candidate)
    else let parent = takeDirectory dir
         in if parent == dir
            then pure Nothing
            else findConfigFile parent

-- | Load and parse a config file.
loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = do
  bs <- BS.readFile path
  pure $ case decodeEither' bs of
    Left err -> Left (show err)
    Right cfg -> Right cfg

-- ## Filtering

-- | Filter unused items according to a category filter.
filterUnusedItems :: Maybe CategoryFilter -> UnusedItems -> UnusedItems
filterUnusedItems Nothing items
  = items
filterUnusedItems (Just filt) (UnusedItems items)
  = UnusedItems (filter (keepItem filt . snd) items)

-- | Filter unused items and files according to a category filter.
filterUnused :: Maybe CategoryFilter -> Unused -> Unused
filterUnused filt (Unused files items)
  = Unused files (filterUnusedItems filt items)

keepItem :: CategoryFilter -> RangeInfo -> Bool
keepItem (Only cats) ri
  = categoryOf ri `elem` cats
keepItem (AllBut cats) ri
  = categoryOf ri `notElem` cats
