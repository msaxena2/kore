module Kore.MatchingLogic.HilbertProof
  (Proof(index,claims,derivations)
  ,ProofSystem(..)
  ,emptyProof
  ,add
  ,derive
  ,renderProof
  )   where
import Data.List(lookup,delete,nub)
import Control.Monad(guard)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Sequence(Seq,(|>))
import qualified Data.Sequence as Seq
import Data.Foldable

data Proof ix rule formula =
  Proof
  { index :: Map ix (Int,formula)
  , claims :: Seq (ix,formula)
  , derivations :: Map ix (rule,[(ix,formula)])
  }
  deriving (Show)

emptyProof :: Proof ix rule formula
emptyProof = Proof Map.empty Seq.empty Map.empty

add :: (Ord ix)
    => Proof ix rule formula -> ix -> formula -> Maybe (Proof ix rule formula)
add proof ix formula
  | not (Map.member ix (index proof)) = Just $
    proof { index = Map.insert ix (Seq.length (claims proof), formula) (index proof)
          , claims = claims proof |> (ix,formula)
          , derivations = derivations proof
          }
  | otherwise = Nothing

renderProof :: (Ord ix, Show ix, Show rule, Show formula)
            => Proof ix rule formula -> String
renderProof proof = unlines
  [show ix++" : "++show formula++
   case Map.lookup ix (derivations proof) of
     Nothing -> ""
     Just (rule,[]) -> " by "++show rule
     Just (rule,arguments) -> " by "++show rule++" from "++
       unwords (map (show . fst) arguments)
  | (ix,formula) <- toList (claims proof)]

class Eq formula => ProofSystem rule formula | rule -> formula where
  checkDerivation :: rule -> formula -> [formula] -> Bool

derive :: (Ord ix, ProofSystem rule formula)
       => Proof ix rule formula
       -> ix -> formula -> rule -> [(ix,formula)]
       -> Maybe (Proof ix rule formula)
derive proof ix f rule arguments = do
  let checkOffset (name,formula) =
        do (offset,formula') <- Map.lookup name (index proof)
           guard (formula == formula')
           return offset
  offset <- checkOffset (ix,f)
  guard $ not (Map.member ix (derivations proof))
  guard $ all (maybe False (< offset) . checkOffset) arguments
  guard $ checkDerivation rule f (map snd arguments)
  return (proof { derivations = Map.insert ix (rule,arguments) (derivations proof) })
