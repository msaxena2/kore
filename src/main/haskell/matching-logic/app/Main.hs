{-# LANGUAGE OverloadedStrings #-}
import           Data.Char                                     (isAlphaNum)
import qualified Data.Map.Strict                               as Map
import qualified Data.Set                                      as Set
import           Data.Text

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Text.Prettyprint.Doc                     (Pretty (..))

import           Data.Reflection

import           Data.Kore.AST.Common                   (SymbolOrAlias (..),
                                                         Variable)
import           Data.Kore.AST.MetaOrObject             (Meta (..),
                                                         Unified (..))
import           Data.Kore.Parser.Parser
import           Kore.MatchingLogic.AST
import           Kore.MatchingLogic.AST.Syntax
import           Kore.MatchingLogic.ProofSystem.MLProofSystem
import           Kore.MatchingLogic.HilbertProof
import           Kore.MatchingLogic.ProofSystem.Minimal
import           Kore.MatchingLogic.ProofSystem.Minimal.Syntax (parseMLRule)
import           Kore.MatchingLogic.ProverRepl
import           Kore.MatchingLogic.Signature.Simple

-- TODO: still needed?
parseName :: Parser String
parseName = takeWhile1P Nothing isAlphaNum <* space

pCommand' :: Parser (Command String (MLRule (SymbolOrAlias Meta) (Variable Meta) CommonMetaPattern) CommonMetaPattern)
pCommand' = parseCommand parseName parseFormula parseRule
  where
    parseFormula = metaPatternParser
    parseLabel   = parseName
    parseSort    = parseName
    parseRule    = parseMLRule metaSymbolOrAliasParser 
                               metaVariableParser 
                               parseFormula 
                               parseName  

proveCommand
    :: IO (ProverState 
            String 
            (MLRule (SymbolOrAlias Meta) (Variable Meta) CommonMetaPattern)
            CommonMetaPattern)
proveCommand = runProver dummyFormulaVerifier pCommand' (ProverState emptyProof)


testSignature :: SignatureInfo
testSignature = SignatureInfo
  { sorts = Set.fromList ["Nat","Bool"]
  , labels = Map.fromList [("plus",("Nat",["Nat","Nat"]))
                          ,("succ",("Nat",["Nat"]))
                          ,("zero",("Nat",[]))
                          ]
  }

main :: IO ()
main = proveCommand >> return ()