module FixOpen where

module M where

  postulate
    A : Set
    B : Set
    C : Set

open M
  using (A; B; C)

postulate
  x : A
  y : C
