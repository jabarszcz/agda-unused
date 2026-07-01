module CrossFileDep where
open import Agda.Builtin.Bool using (Bool; true; false)
x : Bool
x = false

-- 'true' (unused) has byte range 70–74.
-- See CrossFile.agda for the cross-file overlap this tests.
