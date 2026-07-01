module InstanceDef where

postulate

  A : Set

  f
    : A
    → A

private

  postulate

    g
      : A
      → A

  instance

    postulate

      g'
        : A
        → A

    g''
      : A
      → A
    g'' x
      = f x

instance

  postulate

    i : A

h
  : A
  → A
h x
  = f x
