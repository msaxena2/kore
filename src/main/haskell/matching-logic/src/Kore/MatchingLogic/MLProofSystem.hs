module Kore.MatchingLogic.MLProofSystem where
import Kore.MatchingLogic.HilbertProof
import Data.Text

-- Todo: Hook pattern parser
type Pattern = Text
type Var = Text
type Symbol = Text
type Id = Text

data MLRule =
   Propositional1 Pattern Pattern
 | Propositional2 Pattern Pattern Pattern
 | Propositional3 Pattern Pattern
 | ModusPonens Id Id -- ix ix
 | Generalization Var Id -- Var ix
 | VariableSubstitution Var Var Pattern
 | Forall Var Pattern Pattern
 | Necessitation Symbol Int Id --Symbol Int ix
 | PropagateOr Symbol Int Var Pattern
     -- ^ sigma(before ..,\phi1 \/ \phi2,.. after) <->
     --     sigma(before ..,\phi1, .. after) <-> sigma(before ..,\phi2,.. after)
 | PropagateExists Symbol Int Var Pattern
     -- ^ sigma(before ..,Ex x. phi,.. after) <-> Ex x.sigma(before ..,phi,.. after)
 | Existence Var
     -- ^ Ex x.x
 | Singvar Var Pattern [Int] [Int]
    deriving Show

instance ProofSystem MLRule Pattern where
  checkDerivation _ _ _ = True

-- Todo: Replace with actual unparsing
-- instance Show MLRule where
--     show rule = case rule of
--                     (Propositional1 p1 p2)    -> "propositional1(" ++ (show p1) ++ ","  ++ (show p2) ++ ")"
--                     (Propositional2 p1 p2 p3) -> "propositional2(" ++ (show p1) ++ ","  ++ (show p2) ++ (show p3) ++ ")"
