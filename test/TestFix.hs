module TestFix
  ( testApplyFixes
  , testFixInstance
  , testFixSmoke
  ) where

import Agda.Unused
  (UnusedOptions(..))
import Agda.Unused.Fix
  (Edit(..), ImportFix(..), applyFixes, importFixEdits)
import Agda.Unused.Check
  (checkUnusedForFix)
import Agda.Unused.Monad.Error
  (Error)
import Agda.Unused.Monad.Reader
  (Mode(Local))
import Agda.Unused.Print
  (printError)

import qualified Data.Map.Strict
  as Map
import Data.Text
  (Text)
import qualified Data.Text
  as T
import qualified Data.Text.IO
  as TIO
import Control.Exception
  (bracket_)
import System.Directory
  (copyFile, createDirectoryIfMissing, getTemporaryDirectory,
    listDirectory, removeDirectoryRecursive)
import System.FilePath
  ((</>), takeExtension)
import Test.Hspec
  (Expectation, Spec, describe, expectationFailure, it, shouldBe,
    shouldSatisfy)

import Paths_agda_unused
  (getDataFileName)

-- ## applyFixes tests

testApplyFixes :: Spec
testApplyFixes = describe "applyFixes" $ do

  -- Single-line using list tests
  -- For "  using (A; B; C)":
  --   A: col 10-11, B: col 13-14, C: col 16-17

  it "removes middle item" $
    applyFixes "  using (A; B; C)" [DelItem 1 13 14]
      `shouldBe` "  using (A; C)"

  it "removes first item" $
    applyFixes "  using (A; B; C)" [DelItem 1 10 11]
      `shouldBe` "  using (B; C)"

  it "removes last item" $
    applyFixes "  using (A; B; C)" [DelItem 1 16 17]
      `shouldBe` "  using (A; B)"

  it "removes two out of three items" $
    applyFixes "  using (A; B; C)" [DelItem 1 10 11, DelItem 1 16 17]
      `shouldBe` "  using (B)"

  it "removes all items leaving ()" $
    applyFixes "  using (A; B; C)" [DelItem 1 10 11, DelItem 1 13 14, DelItem 1 16 17]
      `shouldBe` "  using ()"

  -- Multiline tests: leading-semicolon layout
  --   using ( A; B        A: line 1 col 11-12, B: line 1 col 14-15
  --         ; C           C: line 2 col 11-12
  --         ; D)          D: line 3 col 11-12

  it "removes item sharing line with (, leading-semicolon" $
    applyFixes
      (T.unlines
        [ "  using ( A; B"
        , "        ; C"
        , "        ; D)"
        ])
      [DelItem 1 11 12]
      `shouldBe`
      T.unlines
        [ "  using ( B"
        , "        ; C"
        , "        ; D)"
        ]

  it "removes item sharing line at end, leading-semicolon" $
    applyFixes
      (T.unlines
        [ "  using ( A; B"
        , "        ; C"
        , "        ; D)"
        ])
      [DelItem 1 14 15]
      `shouldBe`
      T.unlines
        [ "  using ( A"
        , "        ; C"
        , "        ; D)"
        ]

  it "removes item alone on line, leading-semicolon" $
    applyFixes
      (T.unlines
        [ "  using ( A; B"
        , "        ; C"
        , "        ; D)"
        ])
      [DelItem 2 11 12]
      `shouldBe`
      T.unlines
        [ "  using ( A; B"
        , "        ; D)"
        ]

  it "removes item sharing line with ), leading-semicolon" $
    applyFixes
      (T.unlines
        [ "  using ( A; B"
        , "        ; C"
        , "        ; D)"
        ])
      [DelItem 3 11 12]
      `shouldBe`
      T.unlines
        [ "  using ( A; B"
        , "        ; C)"
        ]

  -- Multiline tests: trailing-semicolon layout (with comments on some lines)
  --   using (A ;          A: line 1 col 10-11
  --          B ; -- b     B: line 2 col 10-11
  --          C)           C: line 3 col 10-11

  it "removes item sharing line with (, trailing-semicolon" $
    applyFixes
      (T.unlines
        [ "  using (A ;"
        , "         B ; -- b"
        , "         C)"
        ])
      [DelItem 1 10 11]
      `shouldBe`
      T.unlines
        [ "  using (B ; -- b"
        , "         C)"
        ]

  it "removes item alone on line, trailing-semicolon" $
    applyFixes
      (T.unlines
        [ "  using (A ;"
        , "         B ; -- b"
        , "         C)"
        ])
      [DelItem 2 10 11]
      `shouldBe`
      T.unlines
        [ "  using (A ;"
        , "         C)"
        ]

  it "removes item sharing line with ), trailing-semicolon" $
    applyFixes
      (T.unlines
        [ "  using (A ;"
        , "         B ; -- b"
        , "         C)"
        ])
      [DelItem 3 10 11]
      `shouldBe`
      T.unlines
        [ "  using (A ;"
        , "         B) -- b"
        ]

  -- Ambiguous case: removed item has ; on both adjacent lines.
  -- The choice of which ; to remove is arbitrary (see spec in Fix.hs).
  it "removes item with ; on both adjacent lines" $
    applyFixes
      (T.unlines
        [ "  using (A ; -- a"
        , "         B"
        , "       ; C)"
        ])
      [DelItem 2 10 11]
      `shouldBe`
      T.unlines
        [ "  using (A ; -- a"
        , "         C)"
        ]

  -- ( on separate line

  it "removes first item when ( is on separate line" $
    applyFixes
      (T.unlines
        [ "  using ("
        , "      A"
        , "    ; B; C"
        , "    ; D"
        , "    )"
        ])
      [DelItem 2 7 8]
      `shouldBe`
      T.unlines
        [ "  using ("
        , "      B; C"
        , "    ; D"
        , "    )"
        ]

  -- Remove all items from multiline list

  it "removes all items from multiline list leaving ()" $
    applyFixes
      (T.unlines
        [ "  using ( A; B"
        , "        ; C"
        , "        ; D)"
        ])
      [DelItem 1 11 12, DelItem 1 14 15, DelItem 2 11 12, DelItem 3 11 12]
      `shouldBe`
      "  using ()\n"

  -- Comment tests

  it "preserves comment after )" $
    applyFixes "  using (A; B)  -- the list" [DelItem 1 13 14]
      `shouldBe` "  using (A)  -- the list"

  it "deletes comment on removed item's line" $
    applyFixes
      (T.unlines
        [ "  using ( A; B"
        , "        ; C  -- about C"
        , "        ; D)"
        ])
      [DelItem 2 11 12]
      `shouldBe`
      T.unlines
        [ "  using ( A; B"
        , "        ; D)"
        ]

  -- Comment on the ( line likely describes the import, not the first
  -- item.  B must not be pulled in front of the comment (it would look
  -- like the comment describes B, and B might have its own comment).
  it "preserves comment on ( line when removing first item" $
    applyFixes
      (T.unlines
        [ "  using ( A -- about the module"
        , "        ; B"
        , "        ; C)"
        ])
      [DelItem 1 11 12]
      `shouldBe`
      T.unlines
        [ "  using ( -- about the module"
        , "          B"
        , "        ; C)"
        ]

  -- Multiple using lists in same file

  it "handles multiple using lists in same file" $
    applyFixes
      (T.unlines
        [ "open M"
        , "  using (A; B)"
        , "open N"
        , "  using (C; D)"
        ])
      [DelItem 2 13 14, DelItem 4 13 14]
      `shouldBe`
      T.unlines
        [ "open M"
        , "  using (A)"
        , "open N"
        , "  using (C)"
        ]

  -- Line deletion tests

  it "deletes a single import line range" $
    applyFixes
      (T.unlines
        [ "module Foo where"
        , ""
        , "open import Bar"
        , "  using (X)"
        , ""
        , "A : Set"
        ])
      [DelLines 3 4]
      `shouldBe`
      T.unlines
        [ "module Foo where"
        , ""
        , ""
        , "A : Set"
        ]

  it "deletes multiple line ranges" $
    applyFixes
      (T.unlines
        [ "module Foo where"
        , ""
        , "open import Bar"
        , "  using (X)"
        , "open import Baz"
        , "  using (Y)"
        , ""
        , "A : Set"
        ])
      [DelLines 3 4, DelLines 5 6]
      `shouldBe`
      T.unlines
        [ "module Foo where"
        , ""
        , ""
        , "A : Set"
        ]

  -- Combined item + line tests

  it "applies item edits and line deletions in one pass" $
    applyFixes
      (T.unlines
        [ "module Foo where"            -- 1
        , ""                            -- 2
        , "open import Bar"             -- 3
        , "  using (X)"                 -- 4
        , ""                            -- 5
        , "open import Baz"             -- 6
        , "  using (A; B; C)"           -- 7
        , ""                            -- 8
        , "x = A"                       -- 9
        ])
      [ DelItem 7 16 17   -- remove C from line 7
      , DelLines 3 4    -- delete import Bar
      ]
      `shouldBe`
      T.unlines
        [ "module Foo where"
        , ""
        , ""
        , "open import Baz"
        , "  using (A; B)"
        , ""
        , "x = A"
        ]

  it "handles item edit above line deletion" $
    applyFixes
      (T.unlines
        [ "module Foo where"            -- 1
        , ""                            -- 2
        , "open import Baz"             -- 3
        , "  using (A; B; C)"           -- 4
        , ""                            -- 5
        , "open import Bar"             -- 6
        , "  using (X)"                 -- 7
        , ""                            -- 8
        , "x = A"                       -- 9
        ])
      [ DelItem 4 16 17   -- remove C from line 4
      , DelLines 6 7    -- delete import Bar
      ]
      `shouldBe`
      T.unlines
        [ "module Foo where"
        , ""
        , "open import Baz"
        , "  using (A; B)"
        , ""
        , ""
        , "x = A"
        ]

  it "handles multiline item edit that deletes a line above line deletion" $
    applyFixes
      (T.unlines
        [ "module Foo where"            -- 1
        , ""                            -- 2
        , "open import Baz"             -- 3
        , "  using ( A"                 -- 4
        , "        ; B"                 -- 5
        , "        ; C"                 -- 6
        , "        )"                   -- 7
        , ""                            -- 8
        , "open import Bar"             -- 9
        , "  using (X)"                 -- 10
        , ""                            -- 11
        , "x = A"                       -- 12
        ])
      [ DelItem 5 11 12   -- remove B from line 5 (deletes line)
      , DelLines 9 10   -- delete import Bar (original lines)
      ]
      `shouldBe`
      T.unlines
        [ "module Foo where"
        , ""
        , "open import Baz"
        , "  using ( A"
        , "        ; C"
        , "        )"
        , ""
        , ""
        , "x = A"
        ]

-- ## Fix instance heuristic tests

testFixInstance :: Spec
testFixInstance = describe "fix instance heuristic" $ do

  it "conservatively keeps import when instances detected, deletes items" $
    withFixDir "data/fix" "fix-inst" $ \tmpDir ->
      assertInstancePreserved tmpDir "FixInstanceImport.agda"
        "open import InstanceProvider"

  it "conservatively keeps import re-exporting instances, deletes items" $
    withFixDir "data/fix" "fix-trans" $ \tmpDir ->
      assertInstancePreserved tmpDir "FixTransitiveInstance.agda"
        "open import ReExportInstances"

  it "deletes import when no instances detected" $
    withFixDir "data/fix" "fix-noinst" $ \tmpDir -> do
      result <- checkAndFix tmpDir (tmpDir </> "FixNoInstanceImport.agda")
      case result of
        Left e -> expectationFailure (T.unpack (printError e))
        Right ([DeleteImport _ _ _ _], _) -> pure ()
        Right (fixes, _) ->
          expectationFailure ("Expected [DeleteImport], got " ++ show (length fixes) ++ " fix(es)")

-- | Assert that --fix deletes items but conservatively keeps the import
-- line intact, because the imported module may provide instances
-- (syntactic heuristic; see Agda.Unused.Fix).
assertInstancePreserved :: FilePath -> FilePath -> Text -> Expectation
assertInstancePreserved tmpDir fileName importLine = do
  result <- checkAndFix tmpDir (tmpDir </> fileName)
  case result of
    Left e -> expectationFailure (T.unpack (printError e))
    Right ([DeleteItems _ _ _ _], Just fixed) -> do
      fixed `shouldSatisfy` T.isInfixOf "using ()"
      fixed `shouldSatisfy` T.isInfixOf importLine
    Right (_, Nothing) ->
      expectationFailure "No fixes applied"
    Right (fixes, _) ->
      expectationFailure ("Expected [DeleteItems], got " ++ show (length fixes) ++ " fix(es)")

-- ## Smoke tests

testFixSmoke :: Spec
testFixSmoke = describe "fix smoke tests" $ do

  it "does not crash on declaration fixtures" $
    smokeTestDir "data/declaration"

  it "does not crash on expression fixtures" $
    smokeTestDir "data/expression"

  it "does not crash on pattern fixtures" $
    smokeTestDir "data/pattern"

  it "does not crash on example fixtures" $
    smokeTestDir "data/example"

  it "does not crash on fix fixtures" $
    smokeTestDir "data/fix"

smokeTestDir :: FilePath -> Expectation
smokeTestDir relDir = do
  dataDir <- getDataFileName relDir
  tmpDir <- (</> "agda-unused-smoke") <$> getTemporaryDirectory
  createDirectoryIfMissing True tmpDir
  agdaFiles <- copyFixtures dataDir tmpDir
  bracket_ (pure ()) (removeDirectoryRecursive tmpDir) $
    mapM_ (smokeTestFile tmpDir) agdaFiles

-- | Smoke-test a single file: check, apply fixes, re-check.
-- After applying fixes, a second run should produce no new fixes.
smokeTestFile :: FilePath -> FilePath -> Expectation
smokeTestFile dir fileName = do
  result <- checkAndFix dir (dir </> fileName)
  case result of
    Left _ -> pure ()
    Right (_, Just _) -> do
      result2 <- checkUnusedForFix Local (unusedOptions dir) (dir </> fileName)
      case result2 of
        Left e ->
          expectationFailure
            (fileName ++ ": re-check failed: "
             ++ T.unpack (printError e))
        Right fixes2
          | null fixes2 -> pure ()
          | otherwise ->
              expectationFailure
                (fileName ++ ": re-check still produced fixes: "
                 ++ show fixes2)
    Right _ -> pure ()

-- ## Helpers

unusedOptions :: FilePath -> UnusedOptions
unusedOptions p = UnusedOptions
  { unusedOptionsInclude = [p]
  , unusedOptionsLibraries = []
  , unusedOptionsLibrariesFile = Nothing
  , unusedOptionsUseLibraries = False
  , unusedOptionsUseDefaultLibraries = False
  }

-- | Copy fixtures to a temp dir, run an action, clean up.
withFixDir :: String -> String -> (FilePath -> Expectation) -> Expectation
withFixDir relDir label action = do
  rootPath <- getDataFileName relDir
  tmpDir <- (</> label) <$> getTemporaryDirectory
  createDirectoryIfMissing True tmpDir
  _ <- copyFixtures rootPath tmpDir
  bracket_ (pure ()) (removeDirectoryRecursive tmpDir) $
    action tmpDir

-- | Check a file for unused items and apply any fixes.
checkAndFix
  :: FilePath -> FilePath
  -> IO (Either Error ([ImportFix], Maybe Text))
checkAndFix dir filePath = do
  result <- checkUnusedForFix Local (unusedOptions dir) filePath
  case result of
    Left e -> pure (Left e)
    Right fixes -> do
      let fileEdits = Map.findWithDefault [] filePath (importFixEdits fixes)
      if null fileEdits
        then pure (Right (fixes, Nothing))
        else do
          src <- TIO.readFile filePath
          let fixed = applyFixes src fileEdits
          TIO.writeFile filePath fixed
          pure (Right (fixes, Just fixed))

-- | Copy all .agda files from one directory to another.
copyFixtures :: FilePath -> FilePath -> IO [FilePath]
copyFixtures srcDir dstDir = do
  entries <- listDirectory srcDir
  let files = filter (\f -> takeExtension f == ".agda") entries
  mapM_ (\f -> copyFile (srcDir </> f) (dstDir </> f)) files
  pure files
