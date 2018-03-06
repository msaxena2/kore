module Kore.MatchingLogic.DummyProofSystem
  (DummyRule(DummyRule)
  ) where
import Kore.MatchingLogic.HilbertProof
import Data.Text

newtype DummyRule formula = DummyRule Text
instance Show (DummyRule formula) where
  show (DummyRule rule) = unpack rule

instance (Eq formula) => ProofSystem (DummyRule formula) formula where
  checkDerivation _ _ _ = True
