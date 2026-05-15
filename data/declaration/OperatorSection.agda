module OperatorSection where

open import Agda.Builtin.Nat using (Nat; _+_)

-- Use _+_ in a left section — should NOT be reported unused.
f : Nat → Nat → Nat
f x = x +_
