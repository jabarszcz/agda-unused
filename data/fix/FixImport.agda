module FixImport where

open import Agda.Builtin.Bool
  using (Bool; false; true)
open import Agda.Builtin.Unit
  using (⊤; tt)

A
  : Bool
A
  = false

B
  : ⊤
B
  = tt
