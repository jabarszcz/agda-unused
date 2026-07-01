module AnyWhere where

record Pair (A B : Set) : Set where
  field
    fst : A
    snd : B

swap : {A B : Set} → Pair A B → Pair B A
swap p with fst | snd
         where open Pair p
... | a | b = record { fst = b ; snd = a }

-- 'g' is unused
g : {A : Set} → A → A
g x = x
