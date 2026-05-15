module RecordInstance where

record R : Set₁ where
  field
    A : Set
    B : Set

postulate
  instance r : R

-- SectionApp path (explicit arg): anonymous open, B unused
open R r using (A; B)

postulate
  a : A

-- SectionApp path (explicit arg): named DontOpen, M.A used
module M = R r

postulate
  b : M.A

-- RecordModuleInstance path (⦃ ... ⦄): named DontOpen, N.B used
module N = R ⦃ ... ⦄

postulate
  c : N.B

-- RecordModuleInstance path (⦃ ... ⦄): anonymous DoOpen, open & A unused
open R ⦃ ... ⦄ using (A)

-- RecordModuleInstance path (⦃ ... ⦄): anonymous DoOpen, A' unused but open used
open R ⦃ ... ⦄ renaming (A to A'; B to B')

postulate
  d : B'
