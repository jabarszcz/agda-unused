{- |
Module: Agda.Unused.Print

Printing functions for unused items and errors.
-}
module Agda.Unused.Print
  ( printError
  , printUnused
  , printUnusedItems
  , printNothing
  , relativizeUnused
  , relativizeUnusedItems
  ) where

import Agda.Unused
  (Unused(..), UnusedItems(..))
import Agda.Unused.Monad.Error
  (Error(..), InternalError(..), UnexpectedError(..), UnsupportedError(..))
import Agda.Unused.Types.Name
  (Name(..), NamePart(..), QName(..))
import Agda.Unused.Types.Range
  (RangeInfo(..), RangeType(..))

import Agda.Interaction.FindFile
  (FindError(..))
import Agda.Syntax.Concrete.Definitions.Errors
  (DeclarationException(..))
import Agda.Syntax.Position
  (Range, Range'(..), RangeFile(..), getRange)
import Agda.Syntax.Common.Pretty
  (prettyShow)
import Agda.Utils.FileName
  (AbsolutePath(..), filePath)
import qualified Agda.Utils.Maybe.Strict
  as S
import Data.Semigroup
  (sconcat)
import Data.Text
  (Text)
import qualified Data.Text
  as T
import System.FilePath
  (makeRelative, normalise)

-- ## Utilities

quote
  :: Text
  -> Text
quote t
  = "‘" <> t <> "’"

parens
  :: Text
  -> Text
parens t
  = "(" <> t <> ")"

-- ## Names

printNamePart
  :: NamePart
  -> Text
printNamePart
  = T.pack . prettyShow

printName
  :: Name
  -> Text
printName (Name ps)
  = sconcat (printNamePart <$> ps)

printQName
  :: QName
  -> Text
printQName (QName n)
  = printName n
printQName (Qual n ns)
  = printName n <> "." <> printQName ns

-- ## Ranges

-- | Rewrite absolute paths in a range to be relative to the given base
-- directory. HACK: stuffs a relative path into AbsolutePath so that
-- prettyShow prints short paths.
relativizeRange
  :: FilePath
  -> Range
  -> Range
relativizeRange base (Range (S.Just (RangeFile p m)) is)
  = Range (S.Just (RangeFile (AbsolutePath (T.pack (makeRelative (normalise base) (filePath p)))) m)) is
relativizeRange _ r
  = r

-- | Make paths in unused items relative to the given base directory.
relativizeUnusedItems
  :: FilePath
  -> UnusedItems
  -> UnusedItems
relativizeUnusedItems base (UnusedItems items)
  = UnusedItems (map (\(r, i) -> (relativizeRange base r, i)) items)

-- | Make paths in unused results relative to the given base directory.
relativizeUnused
  :: FilePath
  -> Unused
  -> Unused
relativizeUnused base (Unused files items)
  = Unused files (relativizeUnusedItems base items)

printRange
  :: Range
  -> Text
printRange NoRange
  = "unknown location"
printRange r@(Range _ _)
  = T.pack (prettyShow r)

-- ## Messages

printMessage
  :: Text
  -> Text
  -> Text
printMessage t1 t2
  = T.intercalate "\n" [t1, t2]

-- ## Errors

-- | Print an error.
printError
  :: Error
  -> Text

printError (ErrorAmbiguous r n)
  = printMessage (printRange r)
  $ "Error: Ambiguous name " <> parens (quote (printQName n)) <> "."
printError (ErrorCyclic r n)
  = printMessage (printRange r)
  $ "Error: Cyclic module dependency " <> parens (printQName n) <> "."
printError (ErrorFind r n (NotFound _))
  = printMessage (printRange r)
  $ "Error: Import not found " <> parens (printQName n) <> "."
printError (ErrorFind r n (Ambiguous _))
  = printMessage (printRange r)
  $ "Error: Ambiguous import " <> parens (printQName n) <> "."
printError (ErrorFixity (Just r))
  = printMessage (printRange r)
  $ "Error: Multiple fixity declarations."
printError (ErrorGlobal r)
  = printMessage (printRange r)
  $ "Error: With --global, all declarations in the given file must be imports."
printError (ErrorOpen r n)
  = printMessage (printRange r)
  $ "Error: Module not found " <> parens (printQName n) <> "."
printError (ErrorPolarity (Just r))
  = printMessage (printRange r)
  $ "Error: Multiple polarity declarations."
printError (ErrorRoot n n')
  = printMessage (printQName n)
  $ "Error: Root not found " <> parens (quote (printQName n')) <> "."
printError (ErrorUnsupported e r)
  = printMessage (printRange r)
  $ "Error: " <> printUnsupportedError e <> " not supported."

printError (ErrorDeclaration (DeclarationException _ e))
  = printRange (getRange e) <> "\n" <> T.pack (prettyShow e)
printError (ErrorFile p)
  = printErrorFile p
printError (ErrorFixity Nothing)
  = "Error: Multiple fixity declarations."
printError (ErrorInclude msg)
  = "Error: Invalid path-related options.\n" <> T.pack msg
printError (ErrorInternal e)
  = printInternalError e
printError (ErrorParse e)
  = T.pack (prettyShow e)
printError (ErrorPolarity Nothing)
  = "Error: Multiple polarity declarations."

printErrorFile
  :: FilePath
  -> Text
printErrorFile p
  = "Error: File not found " <> parens (T.pack p) <> "."

printInternalError
  :: InternalError
  -> Text
printInternalError (ErrorConstructor r)
  = printMessage (printRange r)
  $ "Internal error: Invalid data constructor."
printInternalError (ErrorLet r)
  = printMessage (printRange r)
  $ "Internal error: Invalid let statement."
printInternalError (ErrorName r)
  = printMessage (printRange r)
  $ "Internal error: Invalid name."
printInternalError (ErrorRenaming r)
  = printMessage (printRange r)
  $ "Internal error: Invalid renaming directive."
printInternalError (ErrorUnexpected e r)
  = printMessage (printRange r)
  $ "Internal error: Unexpected constructor "
    <> quote (printUnexpectedError e) <> "."

printUnexpectedError
  :: UnexpectedError
  -> Text
printUnexpectedError UnexpectedAbsurd
  = "Absurd"
printUnexpectedError UnexpectedAs
  = "As"
printUnexpectedError UnexpectedDontCare
  = "DontCare"
printUnexpectedError UnexpectedEllipsis
  = "Ellipsis"
printUnexpectedError UnexpectedEqual
  = "Equal"
printUnexpectedError UnexpectedField
  = "Field"
printUnexpectedError UnexpectedNiceFunClause
  = "NiceFunClause"
printUnexpectedError UnexpectedOpApp
  = "OpApp"
printUnexpectedError UnexpectedOpAppP
  = "OpAppP"

printUnsupportedError
  :: UnsupportedError
  -> Text
printUnsupportedError UnsupportedLoneConstructor
  = "Lone constructors"
printUnsupportedError UnsupportedUnquote
  = "Unquoting primitives"

-- ## Unused

-- | Print a collection of unused items and files.
printUnused
  :: Unused
  -> Maybe Text
printUnused (Unused ps is)
  = printUnusedWith
    (printUnusedFiles ps)
    (printUnusedItems is)

printUnusedWith
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
printUnusedWith Nothing Nothing
  = Nothing
printUnusedWith Nothing (Just t2)
  = Just t2
printUnusedWith (Just t1) Nothing
  = Just t1
printUnusedWith (Just t1) (Just t2)
  = Just (T.intercalate "\n" [t1, t2])

printUnusedFiles
  :: [FilePath]
  -> Maybe Text
printUnusedFiles []
  = Nothing
printUnusedFiles ps@(_ : _)
  = Just (T.intercalate "\n" (printUnusedFile <$> ps))

printUnusedFile
  :: FilePath
  -> Text
printUnusedFile p
  = T.pack p <> ": unused file"

-- | Print a collection of unused items.
printUnusedItems
  :: UnusedItems
  -> Maybe Text
printUnusedItems (UnusedItems [])
  = Nothing
printUnusedItems (UnusedItems rs@(_ : _))
  = Just (T.intercalate "\n" (uncurry printRangeInfoWith <$> rs))

-- | Print a message indicating that no unused code was found.
printNothing
  :: Text
printNothing
  = "No unused code."

printRangeInfoWith
  :: Range
  -> RangeInfo
  -> Text
printRangeInfoWith r i
  = printRange r <> ": " <> printRangeInfo i

printRangeInfo
  :: RangeInfo
  -> Text
printRangeInfo (RangeNamed t n)
  = T.unwords ["unused", printRangeType t, quote (printQName n)]
printRangeInfo RangeMutual
  = "unused mutually recursive definition"

printRangeType
  :: RangeType
  -> Text
printRangeType RangeData
  = "data type"
printRangeType RangeDefinition
  = "definition"
printRangeType RangeImport
  = "import"
printRangeType RangeImportItem
  = "imported item"
printRangeType RangeModule
  = "module"
printRangeType RangeModuleItem
  = "module assignment item"
printRangeType RangeOpen
  = "open"
printRangeType RangeOpenItem
  = "opened item"
printRangeType RangePatternSynonym
  = "pattern synonym"
printRangeType RangePostulate
  = "postulate"
printRangeType RangeRecord
  = "record"
printRangeType RangeRecordConstructor
  = "record constructor"
printRangeType RangeVariable
  = "variable"
