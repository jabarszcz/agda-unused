{- |
Module: Agda.Unused.Monad.State

A state monad for determining unused code.
-}
module Agda.Unused.Monad.State

  ( -- * Definitions

    ModuleState(..)
  , State

    -- * Interface

  , stateEmpty
  , stateItems
  , stateItemsUnfiltered
  , stateModules
  , stateModuleStates
  , stateImportRanges

    -- * Get

  , getHash
  , getModule
  , getSources

    -- * Modify

  , modifyInsert
  , modifyDelete
  , modifyBlock
  , modifyCheck
  , modifySources
  , modifyInsertImportRange

  ) where

import Agda.Unused.Monad.Reader
  (Environment, askSkip, askSuppressions)
import Agda.Unused.Suppress
  (isSuppressed)
import Agda.Unused.Types.Context
  (Context)
import Agda.Unused.Types.Name
  (QName)
import Agda.Unused.Types.Range
  (RangeInfo, rangeContains)

import Agda.Syntax.Common
  (ModuleNameHash(..))
import Agda.Syntax.Position
  (Range, Range'(..))
import Agda.TypeChecking.Monad.Base
  (ModuleToSource(..), FileDictWithBuiltins(..))
import Agda.Utils.FileId
  (FileDictBuilder)
import Agda.Utils.FileName
  (AbsolutePath)
import Agda.Utils.Null
  (empty)
import Control.Monad
  (unless)
import Control.Monad.Reader
  (MonadReader)
import Control.Monad.State
  (MonadState, gets, modify)
import Data.Map.Strict
  (Map)
import qualified Data.Map.Strict
  as Map
import Data.Set
  (Set)
import Data.Word
  (Word64)

-- ## Definitions

-- | Cache the results of checking modules. This allows us to:
--
-- - Avoid duplicate computations.
-- - Handle cyclic module dependencies without nontermination.
data ModuleState where

  Blocked
    :: ModuleState

  Checked
    :: !Context
    -> ModuleState

  deriving Show

-- | The current computation state.
data State
  = State
  { stateItems'
    :: !(Map Range RangeInfo)
    -- ^ Ranges for each unused item.
  , stateModules'
    :: !(Map QName ModuleState)
    -- ^ States for each module dependency.
  , stateSources'
    :: !ModuleToSource
    -- ^ A cache of source paths corresponding to certain module names.
  , stateHash
    :: !Word64
    -- ^ An integer to use as the next module hash.
  , stateImportRanges'
    :: !(Map Range QName)
    -- ^ Maps import/open ranges to their module name.
    --   Used by --fix to find the parent import for unused items.
  }

-- ## Interface

-- | Construct an empty state given the primitive library directory.
stateEmpty
  :: AbsolutePath
  -> State
stateEmpty primLibDir
  = State mempty mempty
      (ModuleToSource (FileDictWithBuiltins (empty :: FileDictBuilder) empty primLibDir) Map.empty)
      0 mempty

-- | Get a sorted list of state items, filtering out nested items.
--
-- If one state item contains another (e.g., an @open@ statement containing
-- @using@ directives), then keep only the containing item.
stateItems
  :: State
  -> [(Range, RangeInfo)]
stateItems
  = stateItemsFilter
  . Map.toAscList
  . stateItems'

-- | Get a sorted list of all state items, including nested ones.
-- Used by the fix path which needs both parent import ranges and
-- child item ranges for 'classifyFixes'.
stateItemsUnfiltered
  :: State
  -> [(Range, RangeInfo)]
stateItemsUnfiltered
  = Map.toAscList
  . stateItems'

-- Remove nested items.
stateItemsFilter
  :: [(Range, RangeInfo)]
  -> [(Range, RangeInfo)]
stateItemsFilter []
  = []
stateItemsFilter (i : i' : is) | rangeContains (fst i) (fst i')
  = stateItemsFilter (i : is)
stateItemsFilter (i : i' : is) | rangeContains (fst i') (fst i)
  = stateItemsFilter (i' : is)
stateItemsFilter (i : is)
  = i : stateItemsFilter is

-- | Get a list of visited modules.
stateModules
  :: State
  -> Set QName
stateModules
  = Map.keysSet
  . stateModules'

-- | Get all module states (for extracting instance info).
stateModuleStates
  :: State
  -> Map QName ModuleState
stateModuleStates
  = stateModules'

-- | Get import/open ranges mapped to their module names.
--   Used by --fix to find the parent import for unused items.
stateImportRanges
  :: State
  -> Map Range QName
stateImportRanges
  = stateImportRanges'

stateSources
  :: ModuleToSource
  -> State
  -> State
stateSources ss s
  = s { stateSources' = ss }

stateInsert
  :: Range
  -> RangeInfo
  -> State
  -> State
stateInsert NoRange _ s
  = s
stateInsert r@(Range _ _) i s
  = s { stateItems' = Map.insert r i (stateItems' s) }

stateDelete
  :: Set Range
  -> State
  -> State
stateDelete rs s
  = s { stateItems' = Map.withoutKeys (stateItems' s) rs }

stateModule
  :: QName
  -> State
  -> Maybe ModuleState
stateModule n s
  = Map.lookup n (stateModules' s)

stateBlock
  :: QName
  -> State
  -> State
stateBlock n s
  = s { stateModules' = Map.insert n Blocked (stateModules' s) }

stateCheck
  :: QName
  -> Context
  -> State
  -> State
stateCheck n c s
  = s { stateModules' = Map.insert n (Checked c) (stateModules' s) }

stateIncrementHash
  :: State
  -> State
stateIncrementHash s
  = s { stateHash = succ (stateHash s) }

-- ## Get

-- | Get a fresh hash.
getHash
  :: MonadState State m
  => m ModuleNameHash
getHash = do
  hash
    <- gets stateHash
  _
    <- modify stateIncrementHash
  pure (ModuleNameHash hash)

-- | Get the state of a module.
getModule
  :: MonadState State m
  => QName
  -> m (Maybe ModuleState)
getModule n
  = gets (stateModule n)

-- | Get the cache of source paths.
getSources
  :: MonadState State m
  => m ModuleToSource
getSources
  = gets stateSources'

-- ## Modify

-- | Record a new unused item.
modifyInsert
  :: MonadReader Environment m
  => MonadState State m
  => Range
  -> RangeInfo
  -> m ()
modifyInsert r i = do
  skip <- askSkip
  sm <- askSuppressions
  unless (skip || isSuppressed sm r i) $
    modify (stateInsert r i)

-- | Mark a list of items as used.
modifyDelete
  :: MonadReader Environment m
  => MonadState State m
  => Set Range
  -> m ()
modifyDelete rs
  = askSkip >>= flip unless (modify (stateDelete rs))

-- | Mark that we are beginning to check a module.
modifyBlock
  :: MonadState State m
  => QName
  -> m ()
modifyBlock n
  = modify (stateBlock n)

-- | Record the results of checking a module.
modifyCheck
  :: MonadState State m
  => QName
  -> Context
  -> m ()
modifyCheck n c
  = modify (stateCheck n c)

-- | Update the cache of sources.
modifySources
  :: MonadState State m
  => ModuleToSource
  -> m ()
modifySources ss
  = modify (stateSources ss)

-- | Record an import/open range and its module name.
--   Used by --fix to find the parent import for unused items.
modifyInsertImportRange
  :: MonadState State m
  => Range
  -> QName
  -> m ()
modifyInsertImportRange NoRange _
  = pure ()
modifyInsertImportRange r n
  = modify (\s -> s { stateImportRanges' = Map.insert r n (stateImportRanges' s) })

