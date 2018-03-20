{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kore.MatchingLogic.AST
  ( PatternF(..)
  , Pattern
  , patternSort
  , IsSignature(..)
  , SigPatternF
  , SigPattern
  , WFPattern , fromWFPattern
  , checkSorts1
  , checkSorts
  ) where

import Data.Functor.Classes
import           Data.Typeable (Typeable, cast, typeOf, typeRepArgs)
import Data.Proxy
import Data.Deriving(deriveEq1,deriveShow1)
import Data.Functor.Foldable --(Fix(Fix))
import Data.Coerce
import Data.Text.Prettyprint.Doc

import Kore.MatchingLogic.Signature

data PatternF sort label v p
  = Variable sort v
  | Application label [p]
  | And sort p p
  | Not sort p
  | Exists sort sort v p
  deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

deriveEq1 ''PatternF
deriveShow1 ''PatternF

type SigPatternF sig = PatternF (Sort sig) (Label sig)

type Pattern sort label v = Fix (PatternF sort label v)
type SigPattern sig v = Fix (SigPatternF sig v)

newtype WFPattern sig var = WFPattern {fromWFPattern :: SigPattern sig var}
deriving instance (Eq (Sort sig), Eq (Label sig), Eq var) => Eq (WFPattern sig var)
deriving instance (Pretty (SigPattern sig var)) => Pretty (WFPattern sig var)

instance Pretty (f (Fix f)) => Pretty (Fix f) where
  pretty (Fix ff) = pretty ff

wfPatSort :: (IsSignature sig) => WFPattern sig var -> Sort sig
wfPatSort (WFPattern (Fix p)) = patternSort p

checkSorts1 :: (IsSignature sig, Eq (Sort sig))
            => SigPatternF sig var (WFPattern sig var)
            -> Maybe (WFPattern sig var)

checkSorts1 pat = if patOk then Just (WFPattern (Fix (coerce pat))) else Nothing
  where
    patOk = case pat of
      Variable _ _ -> True
      Application l ps ->
        and (zipWith (==) (labelArguments l) (map wfPatSort ps))
      And _ _ _ -> childrenSame
      Not _ _ -> childrenSame
      Exists _ _ _ _ -> childrenSame
    childrenSame = all (==patternSort pat) (fmap wfPatSort pat)

checkSorts :: (IsSignature sig, Eq (Sort sig))
           => SigPattern sig var
           -> Maybe (WFPattern sig var)
checkSorts pat = cata (\p -> sequenceA p >>= checkSorts1) pat

patternSort :: (IsSignature sig) => SigPatternF sig v p -> Sort sig
patternSort p = case p of
  Variable s _ -> s
  Application l _ -> labelResult l
  And s _ _ -> s
  Not s _ -> s
  Exists s _ _ _ -> s
