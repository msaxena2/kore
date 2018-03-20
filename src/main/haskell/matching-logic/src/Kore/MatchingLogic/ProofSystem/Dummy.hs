module Kore.MatchingLogic.ProofSystem.Dummy
  (DummyRule(DummyRule)
  ) where
import Kore.MatchingLogic.HilbertProof
import Data.Text

newtype DummyRule formula ix = DummyRule Text
  deriving (Functor,Foldable,Traversable)
instance Show (DummyRule formula ix) where
  show (DummyRule rule) = unpack rule

instance (Eq formula) => ProofSystem (DummyRule formula) formula where
  checkDerivation _ _ = True
