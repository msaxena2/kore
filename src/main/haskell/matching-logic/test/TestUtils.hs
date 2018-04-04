{-# LANGUAGE FlexibleInstances #-}
module TestUtils where

import           Control.Applicative                           (some)
import           Data.Text                                     (Text)
import qualified Data.Text                                     as T
import           Data.Void
import           Text.Megaparsec                               hiding (some)
import           Text.Megaparsec.Char

import           Kore.MatchingLogic.AST
import           Kore.MatchingLogic.ProofSystem.Minimal
import           Kore.MatchingLogic.ProofSystem.Minimal.Syntax



type DummyParser = Parsec Void Text

type DummySort  = Text
type DummyLabel = Text
type DummyIx    = Int
type DummyVar   = Text
type DummyTerm  = Text

sortParser       :: DummyParser DummySort
labelParser      :: DummyParser DummyLabel
ixParser         :: DummyParser DummyIx
varParser        :: DummyParser DummyVar
termParser       :: DummyParser DummyTerm
mlRuleTestParser :: DummyParser (MLRule DummySort DummyLabel DummyVar DummyTerm DummyIx)

-- Implementations for Dummy Parsers, shared by tests
sortParser       = T.pack <$> some alphaNumChar
labelParser      = T.pack <$> some alphaNumChar
ixParser         = read   <$> some digitChar
varParser        = T.pack <$> some alphaNumChar
termParser       = T.pack <$> some alphaNumChar
mlRuleTestParser = parseMLRule sortParser labelParser varParser mlPatternParser ixParser

mlTestPatterns :: [DummyParser DummyTerm] 

testPatterns = [  "P"
                , "Q"
                , "R" ]

        

mlTestPatterns = string <$> T.pack <$> testPatterns 

mlPatternParser = choice mlTestPatterns


-- Dummy Rule Type instantiated with Dummy Parsers 
type DummyRule = MLRule DummySort DummyLabel DummyVar DummyTerm DummyIx

instance Eq DummyRule where
  (==) a b = let t = (a, b) in case t of 
                (Propositional1 a1 a2, Propositional1 b1 b2)                   -> a1 == b1   && a2 == b2
                (Propositional1 _ _, _)                                        -> False
                (Propositional2 a1 a2 a3, Propositional2 b1 b2 b3)             -> a1 == b1   && a2 == b2 && a3 == b3
                (Propositional2 _ _ _, _)                                      -> False
                (Propositional3 a1 a2, Propositional3 b1 b2)                   -> a1 == b1   && a2 == b2
                (Propositional3 _ _, _)                                        -> False
                (ModusPonens ia1 ia2, ModusPonens ib1 ib2)                     -> ia1 == ib1 && ia2 == ib2
                (ModusPonens _ _, _)                                           -> False
                (Generalization va1 ia1, Generalization va2 ia2)               -> va1 == va2 && ia1 == ia2
                (Generalization _ _, _)                                        -> False
                (VariableSubstitution a1 a2 a3, VariableSubstitution b1 b2 b3) -> a1 == b1   && a2 == b2 && a3 == b3
                (VariableSubstitution _ _ _, _)                                -> False
                (Forall a1 a2 a3, Forall b1 b2 b3)                             -> a1 == b1   && a2 == b2 && a3 == b3
                (Forall _ _ _, _)                                              -> False
                (Necessitation a1 a2 a3, Necessitation b1 b2 b3)               -> a1 == b1   && a2 == b2 && a3 == b3
                (Necessitation _ _ _, _)                                       -> False
                (PropagateOr a1 a2 a3 a4, PropagateOr b1 b2 b3 b4)             -> a1 == b1   && a2 == b2 && a3 == b3 && a4 == b4
                (PropagateOr _ _ _ _, _)                                       -> False
                (PropagateExists a1 a2 a3 a4, PropagateExists b1 b2 b3 b4)     -> a1 == b1   && a2 == b2 && a3 == b3 && a4 == b4
                (PropagateExists _ _ _ _, _)                                   -> False
                (Existence a1, Existence b1)                                   -> a1 == b1
                (Existence _, _)                                               -> False
                (Singvar a1 a2 a3 a4, Singvar b1 b2 b3 b4)                     -> a1 == b1   && a2 == b2 && a3 == b3 && a4 == b4
                (Singvar _ _ _ _, _)                                           -> False

parseTestRule :: String -> DummyRule

parseTestRule ruleStr = case (parse mlRuleTestParser "" (T.pack ruleStr)) of
                          Right parsedRule -> parsedRule


prop1RuleStr = "propositional1(P, Q)"

prop1RuleAST :: DummyRule
prop1RuleAST =  Propositional1 (T.pack "P") (T.pack "Q")



prop2RuleStr = "propositional2(P, Q, R)"

prop2RuleAST :: DummyRule
prop2RuleAST =  Propositional2 (T.pack "P") (T.pack "Q") (T.pack "R")



prop3RuleStr = "propositional3(P, Q)"

prop3RuleAST :: DummyRule
prop3RuleAST =  Propositional3 (T.pack "P") (T.pack "Q")


mpRuleStr = "mp(1, 2)"

mpRuleAST :: DummyRule 
mpRuleAST = ModusPonens 1 2


ugRuleStr = "ug(x, 1)"

ugRuleAST :: DummyRule
ugRuleAST = Generalization (T.pack "x") 1 


varSubstStr = "varsubst(x, 1, y)"

varSubstAST :: DummyRule
varSubstAST = VariableSubstitution (T.pack "x") 1 (T.pack "y") 


forAllStr = "forall(x, P, Q)"
forAllAST :: DummyRule

forAllAST = Forall (T.pack "x") (T.pack "P") (T.pack "Q") 


necessitationStr = "necessitation(x, 2, 1)"
necessitationAST :: DummyRule

necessitationAST = Necessitation (T.pack "x") 2 1



propagateOrStr = "propagate-or(sigma, 2, P, Q)"

propagateOrAST :: DummyRule
propagateOrAST = PropagateOr (T.pack "sigma") 2 (T.pack "P") (T.pack "Q") 


propagateExistsStr = "propagate-exists(sigma, 2, x, P)"

propagateExistsAST :: DummyRule
propagateExistsAST = PropagateExists (T.pack "sigma") 2 (T.pack "x") (T.pack "P") 


existsStr = "exists(x)"

existsAST :: DummyRule
existsAST = Existence (T.pack "x") 


singvarStr = "singvar(x, P, 1, 1)"
singvarAST :: DummyRule
singvarAST = Singvar (T.pack "x") (T.pack "P") [1] [1]
