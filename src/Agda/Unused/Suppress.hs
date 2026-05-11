{- |
Module: Agda.Unused.Suppress

Filter unused items that have been suppressed with an inline comment.

Supported markers (as the start of a comment's content, after trimming):

* @-- agda-unused: ignore@     — suppress all reports on that line
* @-- agda-unused: instances@  — suppress only whole-import reports
                                  (useful for imports kept solely for instances)
-}
module Agda.Unused.Suppress
  ( Suppression(..)
  , SuppressionMap
  , extractSuppressions
  , isSuppressed
  ) where

import Agda.Unused.Types.Range
  (RangeInfo(..), RangeType(..))

import Agda.Syntax.Parser.Tokens
  (Token(..))
import Agda.Syntax.Position
  (Interval'(iStart'), Range, Range'(..), RangeFile(..), posLine, rStart')
import Agda.Utils.FileName
  (filePath)
import qualified Agda.Utils.Maybe.Strict
  as S

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

-- | The kind of suppression on a given line.
data Suppression
  = SuppressAll
  -- ^ Suppress all reports on this line.
  | SuppressImport
  -- ^ Suppress only whole-import reports on this line.
  deriving (Show, Eq)

-- | Map from file path to line-number-indexed suppressions.
type SuppressionMap = Map FilePath (IntMap Suppression)

-- | Extract suppression markers from a token stream for a given file.
extractSuppressions :: FilePath -> [Token] -> SuppressionMap
extractSuppressions fp tokens =
  case IntMap.fromList (concatMap commentSuppression tokens) of
    m | IntMap.null m -> Map.empty
      | otherwise     -> Map.singleton fp m
  where
    commentSuppression :: Token -> [(Int, Suppression)]
    commentSuppression (TokComment (iv, str)) =
      case parseSuppression str of
        Just s  -> [(fromIntegral (posLine (iStart' iv)), s)]
        Nothing -> []
    commentSuppression _ = []

    -- | Parse a suppression marker from a comment string (includes @--@).
    parseSuppression :: String -> Maybe Suppression
    parseSuppression s = case dropWhile (== ' ') (drop 2 s) of
      s' | isPrefixOf "agda-unused: ignore" s'    -> Just SuppressAll
         | isPrefixOf "agda-unused: instances" s' -> Just SuppressImport
         | otherwise                              -> Nothing

-- | Check whether a single item is suppressed.
isSuppressed :: SuppressionMap -> Range -> RangeInfo -> Bool
isSuppressed sm r ri = case rangeFileLine r of
  Nothing -> False
  Just (fp, line) -> case Map.lookup fp sm of
    Nothing -> False
    Just lm -> case IntMap.lookup (fromIntegral line) lm of
      Nothing             -> False
      Just SuppressAll    -> True
      Just SuppressImport -> isImport ri
  where
    isImport :: RangeInfo -> Bool
    isImport (RangeNamed RangeImport _) = True
    isImport _ = False

    rangeFileLine :: Range -> Maybe (FilePath, Word32)
    rangeFileLine r' = case r' of
      Range (S.Just (RangeFile p _)) _ ->
        case rStart' r' of
          Just pos -> Just (filePath p, posLine pos)
          Nothing  -> Nothing
      _ -> Nothing
