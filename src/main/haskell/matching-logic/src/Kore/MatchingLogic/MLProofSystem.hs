module Kore.MatchingLogic.MLProofSystem where
import Kore.MatchingLogic.HilbertProof
import Data.Text

type Pattern = ()
type Var = Text
type Symbol = ()

data MLRule =
   Propositional1 Pattern Pattern
 | Propositional2 Pattern Pattern Pattern
 | Propositional3 Pattern Pattern
 | ModusPonens Pattern Pattern -- ix ix
 | Generalization Var Pattern -- Var ix
 | VariableSubstitution Var Var Pattern
 | Forall Var Pattern Pattern
 | Necessitation Symbol Int Pattern --Symbol Int ix
 | PropagatenOr Symbol Int Pattern Pattern
     -- ^ sigma(before ..,\phi1 \/ \phi2,.. after) <->
     --     sigma(before ..,\phi1, .. after) <-> sigma(before ..,\phi2,.. after)
 | PropagateExists Symbol Int Var Pattern
     -- ^ sigma(before ..,Ex x. phi,.. after) <-> Ex x.sigma(before ..,phi,.. after)
 | Existence Var
     -- ^ Ex x.x
 | Singvar Var Pattern [Int] [Int]

instance ProofSystem MLRule Pattern where
  checkDerivation _ _ _ = True
