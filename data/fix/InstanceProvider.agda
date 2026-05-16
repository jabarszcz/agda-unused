module InstanceProvider where

postulate
  A : Set

record HasDefault : Set where
  field
    theDefault : A

instance
  postulate defaultA : HasDefault
