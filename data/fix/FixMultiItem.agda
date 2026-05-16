module FixMultiItem where

-- Both Nat and suc are unused (only used qualified via alias).
-- Triggers the applyFixes bug: multiple items on the same using-list line.
open import Agda.Builtin.Nat as Nat using (Nat; suc)

f : Nat.Nat
f = Nat.suc Nat.zero
