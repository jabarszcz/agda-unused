{-# LANGUAGE CPP #-}
{- |
Module: Agda.Unused.Fix

Auto-fix unused import/open items.
-}
module Agda.Unused.Fix

  ( -- * Text surgery

    Edit(..)
  , applyFixes

    -- * High-level fix API

  , ImportFix(..)
  , ImportRanges
  , classifyFixes
  , importFixEdits
  , printImportFix

  ) where

import Agda.Unused.Print
  (printQName)
import Agda.Unused.Types.Name
  (QName)
import Agda.Unused.Types.Range
  (RangeInfo(..), RangeType(..), rangeContains, rangePath)

import Agda.Syntax.Position
  (Range, rStart', rEnd', posLine, posCol)
import Data.Word
  (Word32)
#if !MIN_VERSION_base(4,20,0)
import Data.List
  (foldl')
#endif
import Data.List
  (sortOn)
import qualified Data.Map.Strict
  as Map
import Data.Ord
  (Down(..))
import Data.Set
  (Set)
import qualified Data.Set
  as Set
import Data.Text
  (Text)
import qualified Data.Text
  as T

-- ## Text surgery
--
-- Item removal from @using (...)@ lists.  @hiding@ and @renaming@
-- lists are not yet handled (@renaming@ items have non-trivial
-- content, should we silently discard them?).
--
-- Goal: remove one named item and its separator, producing valid
-- output.  Whole-import deletion is a separate, simpler operation
-- ('DelLines').
--
-- Invariants:
--
--   * The item and its associated @;@ are removed together.
--   * Parentheses @(@ and @)@ are always preserved.
--   * If the item (with its @;@) was the only meaningful content on
--     its line, the entire line is deleted.
--   * Removing an item never leaves an orphaned @;@ on an adjacent
--     line (leading @;@ after @(@ is invalid, trailing @;@ before
--     @)@ is untidy).  When the removed item had @;@ on both
--     adjacent lines, the choice of which to remove is arbitrary
--     (current implementation removes the one below; this is an
--     implementation detail, not a semantic requirement).
--   * Best effort to preserve the original formatting style:
--     indentation, placement of @(@ and @)@, leading vs. trailing
--     @;@.
--
-- Comments:
--
--   Only comments on the same line as the removed item are deleted
--   (they almost certainly described it).  Comments on separate lines
--   are never claimed, since we cannot reliably determine what they
--   refer to.  Comments after @)@ and on the @(@ line are preserved:
--   they probably describe the using-list or import, not an
--   individual item.

-- | A single text edit.
data Edit
  = DelItem  !Int !Int !Int
    -- ^ Remove an item from a using list (line, startCol, endCol).
    --   Lines and columns are 1-based; endCol is exclusive.
  | DelLines !Int !Int
    -- ^ Delete a range of lines (startLine, endLine), both inclusive.
  deriving (Show, Eq, Ord)

-- | Apply edits to source text in one pass.
-- Edits are sorted by (line, col) descending so each edit operates on
-- text that has not been shifted by earlier edits.
applyFixes :: Text -> [Edit] -> Text
applyFixes src edits
  = let sorted = sortOn (Down . editKey) edits
        ls = T.lines src
        hasTrailingNL = not (T.null src) && T.last src == '\n'
    in joinLines (foldl' applyEdit ls sorted) hasTrailingNL
  where
    editKey (DelItem line startCol _) = (line, startCol)
    editKey (DelLines startLine _) = (startLine, 0)

    applyEdit ls (DelItem line startCol endCol)
      = removeItem ls (line - 1) startCol endCol
    applyEdit ls (DelLines s e)
      = take (s - 1) ls ++ drop e ls

    joinLines ls hasTrailingNL
      = let body = T.intercalate "\n" ls
        in if hasTrailingNL then body <> "\n" else body

-- ## High-level fix API

-- | Import/open ranges mapped to their module names.
--   Used by 'classifyFixes' to find the parent import for unused items.
type ImportRanges
  = Map.Map Range QName

-- | A classified fix for one import statement.
data ImportFix
  = DeleteImport !RangeType !QName !FilePath !Edit
    -- ^ Whole import/open deleted (no instances detected, all items unused).
  | DeleteItems !RangeType !QName !FilePath ![(QName, Edit)]
    -- ^ Items removed from an import/open. Range type, module name,
    --   file path, and list of (item name, edit) pairs.
  deriving Show

-- | Classify unused items into fixable actions.
--
-- Instance detection is a syntactic heuristic: we check whether an
-- @instance@ block appeared in the module or was re-exported via
-- @public@, but we cannot verify that the instances are actually used.
-- When in doubt, we conservatively keep the import and only remove
-- individual items.
--
-- Input items must be sorted by range (ascending), as returned by
-- @stateItems@. The list may contain both whole-import ranges
-- (@RangeImport@, @RangeOpen@) and their contained item ranges
-- (@RangeImportItem@, @RangeOpenItem@).
--
-- Items that don't map to any 'ImportFix' are silently dropped;
-- the caller should re-check the file after applying fixes to find
-- remaining issues with fresh positions.
classifyFixes :: Set QName -> ImportRanges -> [(Range, RangeInfo)] -> [ImportFix]
classifyFixes instanceMods importRanges = go Nothing
  where
    go _ [] = []
    go ctx ((r, ri) : rest)
      -- Whole-import, no instances detected: delete the whole thing.
      | isWholeImport ri
      , not (hasInstances ri)
      , RangeNamed rt modName <- ri
      , Just ed <- rangeToDelLines r
      , Just path <- rangePath r
      = DeleteImport rt modName path ed : go (Just (r, rt, modName, True)) rest

      -- Whole-import, may have instances: conservatively keep import.
      | isWholeImport ri
      , hasInstances ri
      , RangeNamed rt modName <- ri
      = go (Just (r, rt, modName, False)) rest

      -- Whole-import we can't handle: skip, don't track as parent.
      | isWholeImport ri
      = go Nothing rest

      -- Item inside a deleted import: skip.
      | Just (parent, _, _, True) <- ctx
      , rangeContains parent r
      = go ctx rest

      -- Item inside a possibly-instance import: delete item, keep import.
      | Just (parent, rt, modName, False) <- ctx
      , rangeContains parent r
      , isFixable ri
      , Just ed <- rangeToDelItem r
      , Just path <- rangePath r
      , RangeNamed _ itemName <- ri
      = addDeletedItem rt modName path itemName ed (go ctx rest)

      -- Standalone fixable item (parent import was used, not in list).
      | isFixable ri
      , Just ed <- rangeToDelItem r
      , Just path <- rangePath r
      , RangeNamed rt itemName <- ri
      , Just parentRt <- itemToParentType rt
      , Just modName <- lookupImportRange r
      = addDeletedItem parentRt modName path itemName ed (go ctx rest)

      -- Anything else: not fixable, skip.
      | otherwise
      = go ctx rest

    lookupImportRange itemR
      = case filter (\(ir, _) -> rangeContains ir itemR) (Map.toList importRanges) of
          [(_, modName)] -> Just modName
          _              -> Nothing

    isWholeImport (RangeNamed RangeImport _) = True
    isWholeImport (RangeNamed RangeOpen _) = True
    isWholeImport _ = False

    isFixable (RangeNamed RangeImportItem _) = True
    isFixable (RangeNamed RangeOpenItem _) = True
    isFixable _ = False

    hasInstances (RangeNamed _ n) = Set.member n instanceMods
    hasInstances _ = False

    itemToParentType RangeImportItem = Just RangeImport
    itemToParentType RangeOpenItem = Just RangeOpen
    itemToParentType _ = Nothing

    addDeletedItem _ modName path itemName ed (DeleteItems rt' m p ns : rest)
      | m == modName, p == path
      = DeleteItems rt' m p ((itemName, ed) : ns) : rest
    addDeletedItem rt modName path itemName ed fixes
      = DeleteItems rt modName path [(itemName, ed)] : fixes

    rangeToDelLines r = do
      s <- rStart' r
      e <- rEnd' r
      Just (DelLines (w32 (posLine s)) (w32 (posLine e)))

    rangeToDelItem r = do
      s <- rStart' r
      e <- rEnd' r
      Just (DelItem (w32 (posLine s)) (w32 (posCol s)) (w32 (posCol e)))

    w32 :: Word32 -> Int
    w32 = fromIntegral

-- | Extract the edits from a list of import fixes, grouped by file.
importFixEdits :: [ImportFix] -> Map.Map FilePath [Edit]
importFixEdits = Map.fromListWith (++) . map fixEdits
  where
    fixEdits (DeleteImport _ _ path ed) = (path, [ed])
    fixEdits (DeleteItems _ _ path items) = (path, map snd items)

-- | Print a human-readable summary of an import fix.
printImportFix :: ImportFix -> Text
printImportFix fix = case fix of
  DeleteImport rt modName _ _ ->
    keyword rt <> printQName modName <> ": deleted"
  DeleteItems rt modName _ items ->
    keyword rt <> printQName modName <> ": removed "
    <> T.intercalate ", " (quote . printQName . fst <$> items)
  where
    keyword RangeImport = "import "
    keyword RangeOpen = "open "
    keyword _ = ""
    quote t = "‘" <> t <> "’"

-- ## Text surgery internals

-- | Remove one item from a using list.
--
-- Three steps:
--
--   1. __Splice__ – remove the item text, producing @before@ (text
--      left of the item) and @after@ (text right of the item).
--      Always the same operation regardless of layout.
--
--   2. __Fix ;__ – the splice leaves an orphaned @;@ at the
--      @before@/@after@ boundary (e.g. @A;  ; C@, or @(  ; B@, or
--      @A;  )@).  Remove it by inspecting the boundary.  When @;@
--      appears on both sides, prefer the left one (makes every
--      non-first item uniform).  If no @;@ is at the boundary,
--      it is on an adjacent line – handled in step 3.
--
--   3. __Tidy__ – inspect the resulting line.  If trivial, delete it
--      (and strip the adjacent-line @;@ if step 2 didn't find one).
--      If only @)@ remains, move it up.  If only @(@ remains (with
--      or without a comment), pull up the next line or strip its @;@.
--      If @( )@, collapse to @()@.  Otherwise keep the line.
removeItem
  :: [Text]  -- ^ lines of the file
  -> Int     -- ^ 0-based line index
  -> Int     -- ^ 1-based start column (inclusive)
  -> Int     -- ^ 1-based end column (exclusive)
  -> [Text]
removeItem ls idx sc ec
  | idx < 0 || idx >= length ls =
    error $ "removeItem: line index out of bounds: " ++ show idx
  | otherwise =
    let ln    = ls !! idx
        (code, comment) = splitComment ln

        -- Step 1: splice out the item text.
        before = T.take (sc - 1) code
        after  = T.drop (ec - 1) code

        -- Step 2: remove the orphaned ; at the boundary.
        -- Look at the last non-space of before and first non-space of after.
        beforeR = T.dropWhileEnd isHSpace before
        afterL  = T.dropWhile isHSpace after
        (cleaned, hadSemi) = case (T.unsnoc beforeR, T.uncons afterL) of
          -- Prefer left ;.
          (Just (bRest, ';'), _) ->
            (T.dropWhileEnd isHSpace bRest <> after, True)
          (_, Just (';', aRest)) ->
            (before <> T.dropWhile isHSpace aRest, True)
          _ ->
            (before <> after, False)

        -- Is this the first item in the list (immediately after '(')?
        -- If so, record the 0-based index of '(' in cleaned.
        -- When isFirst, beforeR ends with '(' so the left-; branch of
        -- step 2 cannot fire, meaning cleaned always starts with before.
        openPos = case T.unsnoc beforeR of
                    Just (_, '(') -> Just (T.length beforeR - 1)
                    _             -> Nothing

        -- Step 3: tidy the result.
        -- What meaningful content remains (skip whitespace and orphaned ;)?
        residue = T.dropWhile (\c -> isHSpace c || c == ';') cleaned

    in tidyLine ls idx cleaned comment residue hadSemi openPos

-- | Tidy the line after splicing and @;@-fixing.  All patterns are
-- detected from the result text alone (no scan context needed).
tidyLine
  :: [Text]  -- ^ lines of the file
  -> Int     -- ^ 0-based line index
  -> Text    -- ^ code after splice + ; fix
  -> Text    -- ^ comment (preserved from original line)
  -> Text    -- ^ residue: code after dropping leading whitespace and @;@
  -> Bool    -- ^ whether a @;@ was already consumed in step 2
  -> Maybe Int  -- ^ 0-based index of @(@ in cleaned, when the removed item was first
  -> [Text]
tidyLine ls idx cleaned comment residue hadSemi openPos

  -- Nothing meaningful: delete the line.
  -- If step 2 didn't consume a ;, strip the one on an adjacent line.
  | T.null residue || T.isPrefixOf "--" residue =
    let ls' = deleteAt ls idx
    in if hadSemi then ls'
       else if idx < length ls' && hasLeadingSemi (ls' !! idx)
            then replaceAt ls' idx (stripLeadingSemiToSpace (ls' !! idx))
            else if idx > 0 && hasTrailingSemi (ls' !! (idx - 1))
                 then replaceAt ls' (idx - 1) (stripTrailingSemi (ls' !! (idx - 1)))
                 else ls'

  -- First item was removed: ( position is known from openPos.
  | Just p <- openPos
  , let upToOpen     = T.take (p + 1) cleaned  -- includes the (
        afterOpen    = T.drop (p + 1) cleaned
        afterTrimmed = T.dropWhile (\c -> isHSpace c || c == ';') afterOpen =
    case T.uncons afterTrimmed of
      -- ( … ) with nothing in between → ().
      Just (')', _) ->
        replaceAt ls idx (upToOpen <> ")" <> comment)
      -- Content after (: keep the line.
      Just _ ->
        replaceAt ls idx (cleaned <> comment)
      -- Dangling (: keep comment and strip next ;, or pull up.
      Nothing
        | not (T.null comment) ->
          let trimmed = T.dropWhileEnd isHSpace cleaned
              ls' = replaceAt ls idx (trimmed <> " " <> comment)
          in if idx + 1 < length ls'
             then replaceAt ls' (idx + 1) (stripLeadingSemiToSpace (ls' !! (idx + 1)))
             else ls'
        | idx + 1 < length ls ->
          let nxt     = ls !! (idx + 1)
              s       = T.dropWhile isHSpace nxt
              content = case T.uncons s of
                          Just (';', rest) -> T.dropWhile isHSpace rest
                          _                -> s
          in deleteAt (replaceAt ls idx (cleaned <> content)) (idx + 1)
        | otherwise -> replaceAt ls idx cleaned

  -- Orphaned ): move to end of previous line.
  | Just (')', rest) <- T.uncons residue
  , T.all isHSpace rest
  , idx > 0 =
    let prev = stripTrailingSemi (ls !! (idx - 1))
        (prevCode, prevComment) = splitComment prev
        prevCode' = T.dropWhileEnd isHSpace prevCode
        sep = if T.null prevComment then T.empty else " "
    in deleteAt (replaceAt ls (idx - 1) (prevCode' <> ")" <> sep <> prevComment)) idx

  -- Content remains: keep the edited line.
  | otherwise =
    replaceAt ls idx (cleaned <> comment)

-- ## Small helpers

-- | Split a line into code and comment portions.  The comment portion
-- includes the @--@ prefix and any whitespace before it.
--
-- This is a rough heuristic: it does not handle string literals or
-- nested @{- -}@ block comments.  Good enough for import item lists.
splitComment :: Text -> (Text, Text)
splitComment t = go 0
  where
    len = T.length t
    go i
      | i >= len = (t, T.empty)
      | T.index t i == '-'
      , i + 1 < len
      , T.index t (i + 1) == '-' =
          (T.take i t, T.drop i t)
      | otherwise = go (i + 1)

hasLeadingSemi :: Text -> Bool
hasLeadingSemi t = case T.uncons (T.dropWhile isHSpace t) of
  Just (';', _) -> True
  _             -> False

hasTrailingSemi :: Text -> Bool
hasTrailingSemi t =
  let (code, _) = splitComment t
      trimmed = T.dropWhileEnd isHSpace code
  in case T.unsnoc trimmed of
       Just (_, ';') -> True
       _             -> False

-- | Replace a leading @;@ with a space to preserve column alignment.
stripLeadingSemiToSpace :: Text -> Text
stripLeadingSemiToSpace t =
  let (spaces, rest) = T.span isHSpace t
  in case T.uncons rest of
       Just (';', after) -> spaces <> T.cons ' ' after
       _                 -> t

-- | Remove a trailing @;@ and any whitespace before it,
-- preserving any trailing comment.
stripTrailingSemi :: Text -> Text
stripTrailingSemi t =
  let (code, comment) = splitComment t
      trimmed = T.dropWhileEnd isHSpace code
  in case T.unsnoc trimmed of
       Just (rest, ';') -> T.dropWhileEnd isHSpace rest <> comment
       _                -> trimmed <> comment

replaceAt :: [Text] -> Int -> Text -> [Text]
replaceAt ls i t = take i ls ++ [t] ++ drop (i + 1) ls

deleteAt :: [Text] -> Int -> [Text]
deleteAt ls i = take i ls ++ drop (i + 1) ls

skipHSpaces :: Text -> Int -> Int
skipHSpaces ln i
  | i >= T.length ln = i
  | isHSpace (T.index ln i) = skipHSpaces ln (i + 1)
  | otherwise = i

isHSpace :: Char -> Bool
isHSpace ' '  = True
isHSpace '\t' = True
isHSpace _    = False

