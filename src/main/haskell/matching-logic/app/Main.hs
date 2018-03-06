module Main where
import Data.Char(isSpace)
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

parseId :: Parser Text
parseId = pack <$> some (satisfy (\c -> not (isSpace c) && c /= ':'))

parseFormula :: Parser Text
parseFormula =
  pack <$> some (satisfy (\c -> not (isSpace c) && c /= ':'))

parseDerivation :: Parser (DummyRule Text,[Text])
parseDerivation = do
  rule <- pack <$> some (satisfy (\c -> not (isSpace c) && c /= ':'))
  spaces
  option (DummyRule rule,[]) (do
    string "from"
    argIds <- many (spaces *> parseId)
    return (DummyRule rule,argIds))

pCommand :: Parser (Command Text (DummyRule Text) Text)
pCommand = parseCommand parseId parseFormula parseDerivation <* eof

{- defaultSettings -}

type ProverState = ()

banner :: InputT IO ()
banner = outputStrLn "Welcome to the matching logic prover"

main :: IO ()
main = do
  runProver pCommand (ProverState emptyProof)
  return ()
