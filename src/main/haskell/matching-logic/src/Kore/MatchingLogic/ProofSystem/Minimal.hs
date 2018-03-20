module Kore.MatchingLogic.ProofSystem.Minimal where
import           Data.Text
import           Kore.MatchingLogic.AST             as AST
import           Kore.MatchingLogic.HilbertProof
import Data.Functor.Foldable(Fix(..))

type Var = Text
type Id = Text

data MLRule sort label var term hypothesis =
   Propositional1 term term
 | Propositional2 term term term
 | Propositional3 term term
 | ModusPonens hypothesis hypothesis
 | Generalization var hypothesis
 | VariableSubstitution var hypothesis var
 | Forall var term term
 | Necessitation label Int hypothesis
 | PropagateOr label Int term term
     -- ^ sigma(before ..,\phi1 \/ \phi2,.. after) <->
     --     sigma(before ..,\phi1, .. after) <-> sigma(before ..,\phi2,.. after)
 | PropagateExists label Int var term
     -- ^ sigma(before ..,Ex x. phi,.. after) <-> Ex x.sigma(before ..,phi,.. after)
 | Existence var
     -- ^ Ex x.x
 | Singvar var term [Int] [Int]
 deriving (Functor, Foldable, Traversable, Show)

type MLRuleSig sig var = MLRule (Sort sig) (Label sig) var (SigPattern sig var)

notPat :: (IsSignature sig)
       => Sort sig -> SigPattern sig var -> SigPattern sig var
notPat s p = Fix (AST.Not s p)

andPat,orPat :: (IsSignature sig)
             => Sort sig -> SigPattern sig var -> SigPattern sig var -> SigPattern sig var
andPat s p1 p2 = Fix (AST.And s p1 p2)
orPat s p1 p2 = notPat s (andPat s (notPat s p1) (notPat s p2))
implies s p1 p2 = orPat s (notPat s p1) p2

patSort :: (IsSignature sig)
        => SigPattern sig v -> Sort sig
patSort (Fix pat) = patternSort pat

instance (IsSignature sig, Eq (Sort sig), Eq (Label sig), Eq var) =>
         ProofSystem (MLRuleSig sig var) (WFPattern sig var) where
  checkDerivation conclusion rule = case rule of
    Propositional1 phi1 phi2
      | patSort phi1 == patSort phi2
        -> let s = patSort phi1
               statement = implies s phi1 (implies s phi2 phi1)
           in fromWFPattern conclusion == statement
    Propositional2 phi1 phi2 phi3
      | patSort phi1 == patSort phi2
      , patSort phi1 == patSort phi3
        -> let s = patSort phi1
               statement = implies s phi1 (implies s phi2 phi1)
           in fromWFPattern conclusion == statement
    _ -> False

-- Todo: Replace with actual unparsing
-- instance Show MLRule where
--     show rule = case rule of
--                     (Propositional1 p1 p2)    -> "propositional1(" ++ (show p1) ++ ","  ++ (show p2) ++ ")"
--                     (Propositional2 p1 p2 p3) -> "propositional2(" ++ (show p1) ++ ","  ++ (show p2) ++ (show p3) ++ ")"
