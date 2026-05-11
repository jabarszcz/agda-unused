{-# OPTIONS --rewriting #-}
module SuppressDefinition where

open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Nat using (_+_)

private
  postulate
    +0 : ∀ n → n + 0 ≡ n -- agda-unused: ignore
    +0' : ∀ n → n + 0 ≡ n

{-# BUILTIN REWRITE _≡_ #-}
{-# REWRITE +0 #-}
