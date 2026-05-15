module SubModuleOpen where

-- open import followed by open of a sub-module.
-- Opening the sub-module should mark the import as used.
open import QualifiedHelper

open QualifiedHelper.Sub

f : A
f = a
