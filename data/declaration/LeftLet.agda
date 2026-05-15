module LeftLet where

record Pair (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B

-- Only used in the 'using' expression; tests that checkExpr marks it used.
wrap : {A B : Set} → Pair A B → Pair A B
wrap p = p

-- 'a' is used in the body, 'b' is not.
-- Tests both used and unused pattern variables in the same 'using' clause.
-- 'wrap' is used in the expression; tests expression-side name tracking.
f : {A B : Set} → Pair A B → A
f p using (a , b) ← wrap p = a
