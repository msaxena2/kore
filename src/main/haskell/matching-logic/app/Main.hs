module Main where
import Data.Char(isSpace, isAlphaNum)
import Control.Applicative(some)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.State.Strict(StateT,runStateT,MonadState(..))
import Data.List(isPrefixOf,isSuffixOf)
import Text.Parsec
import Text.Parsec.String
import Data.Text

import System.Console.Haskeline

import Kore.MatchingLogic.ProverRepl
import Kore.MatchingLogic.HilbertProof
import Kore.MatchingLogic.DummyProofSystem
import Kore.MatchingLogic.MLProofSystem
import Kore.MatchingLogic.MLProofSystemParsers(parseId,parseFormula,parseMLDerivation)

-- Todo: Parsing Formula as Text. Hook to Kore Parser
pCommand :: Parser (Command Text MLRule Text)
pCommand = parseCommand parseId parseFormula parseMLDerivation <* eof

{- defaultSettings -}

type ProverState = ()

banner :: InputT IO ()
banner = outputStrLn "Welcome to the matching logic prover"

main :: IO ()
main = do
  runProver pCommand (ProverState emptyProof)
  return ()
