module NiceFunClause where

record _×_ (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B

data Id {A : Set} : A → A → Set where
  refl : {x : A} → Id x x

-- Only used in the let-pattern RHS; tests that NiceFunClause's checkRHS
-- marks it as used.
split : {A B : Set} → A × B → A × B
split p = p

-- 'a' is used in the return type; 'b' is not.
-- Tests that let-pattern bindings are exported to subsequent declarations.
f : {A B : Set} → (p : A × B) → (let (a , b) = split p) → Id a a
f _ = refl
