module CrossFile where

import CrossFileDep
open import Agda.Builtin.Bool

-- 'open import Agda.Builtin.Bool' has byte range 45–74, which
-- contains CrossFileDep's 'true' at 70–74.  Without the fix (setting
-- module names on RangeFile), Ord treats ranges from both files as
-- same-file, and stateItemsFilter drops 'true' because its range is
-- contained by the import range.
