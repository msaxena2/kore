{-# LANGUAGE TypeFamilies #-}
module Kore.MatchingLogic.Signature where

class IsSignature sig where
  data Label sig :: *
  data Sort sig :: *
  labelSignature :: Label sig -> (Sort sig,[Sort sig])
  labelResult :: Label sig -> Sort sig
  labelResult l = fst (labelSignature l)
  labelArguments :: Label sig -> [Sort sig]
  labelArguments l = snd (labelSignature l)

