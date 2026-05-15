module QualifiedName where

-- suc is imported but only used qualified as N.suc.
-- Only suc (as an unused unqualified item) should be reported.
open import Agda.Builtin.Nat as N using (suc)

postulate
  n : N.Nat

f : N.Nat
f = N.suc n
