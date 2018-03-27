{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Data.Char(isSpace, isAlphaNum)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Applicative(some)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.State.Strict(StateT,runStateT,MonadState(..))
import Data.List(isPrefixOf,isSuffixOf)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text

import Data.Text.Prettyprint.Doc hiding (space)
import qualified Data.Text.Prettyprint.Doc as Doc

import Data.Reflection

import System.Console.Haskeline

import Kore.MatchingLogic.ProverRepl
import Kore.MatchingLogic.HilbertProof
import Kore.MatchingLogic.ProofSystem.Minimal
import Kore.MatchingLogic.ProofSystem.Minimal.Syntax(parseId,parseMLRuleSig)
import Kore.MatchingLogic.AST
import Kore.MatchingLogic.AST.Syntax
import Kore.MatchingLogic.Signature.Simple

-- Todo: Parsing Formula as Text. Hook to Kore Parser
parseName :: Parser Text
parseName = takeWhile1P Nothing isAlphaNum <* space

pCommand :: (Reifies s ValidatedSignature)
         => Parser (Command Text
                    (MLRuleSig (SimpleSignature s) Text)
                    (WFPattern (SimpleSignature s) Text))
pCommand = parseCommand parseName parseFormula parseRule
  where
    parseFormula = simpleSigPattern parseName parseName parseName
    parseLabel = simpleSigLabel parseName
    parseSort = simpleSigSort parseName
    parseRule = parseMLRuleSig parseSort parseLabel parseName parseName


proveCommand :: (Reifies sig ValidatedSignature)
             => proxy (SimpleSignature sig)
             -> IO (ProverState Text (MLRuleSig (SimpleSignature sig) Text) (WFPattern (SimpleSignature sig) Text))
proveCommand _ = runProver pCommand (ProverState emptyProof)

banner :: InputT IO ()
banner = outputStrLn "Welcome to the matching logic prover"

testSignature :: SignatureInfo
testSignature = MkSignatureInfo
  { sorts = Set.fromList ["Nat","Bool"]
  , labels = Map.fromList [("plus",("Nat",["Nat","Nat"]))
                          ,("succ",("Nat",["Nat"]))
                          ,("zero",("Nat",[]))
                          ]
  }

main :: IO ()
main = case validate testSignature of
  Nothing -> return ()
  Just validSig -> reifySignature validSig (\proxy -> proveCommand proxy >> return ())
