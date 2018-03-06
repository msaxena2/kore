module Kore.MatchingLogic.ProverRepl where
import Kore.MatchingLogic.HilbertProof

import System.Console.Haskeline
import Control.Monad.IO.Class(liftIO)
import Control.Monad.State.Strict(StateT,execStateT,MonadState(..),modify')
import Control.Monad.Trans(MonadTrans(lift))
import qualified Data.Map.Strict as Map
import Data.List(isPrefixOf,isSuffixOf)
import Data.Text(Text,pack)
import Text.Parsec
import Text.Parsec.String

newtype ProverState ix rule formula =
  ProverState (Proof ix rule formula)

data Command id rule formula =
   Add id formula
 | Derive id formula rule [id]
 deriving Show

applyCommand :: (Ord id, ProofSystem rule formula)
             => Command id rule formula
             -> Proof id rule formula
             -> Maybe (Proof id rule formula)
applyCommand command proof = case command of
  Add id f -> add proof id f
  Derive id f rule argIds -> do
    argTerms <- traverse (\ix -> fmap snd (Map.lookup ix (index proof))) argIds
    derive proof id f rule (zip argIds argTerms)

parseCommand :: Parser id -> Parser formula -> Parser (rule,[id]) -> Parser (Command id rule formula)
parseCommand pId pFormula pDerivation = do
  id <- pId
  spaces
  char ':'
  spaces
  formula <- pFormula
  spaces
  option (Add id formula)
    (do string "by"
        spaces
        (rule,argIds) <- pDerivation
        return (Derive id formula rule argIds))

runProver :: (Ord ix, ProofSystem rule formula, Show ix, Show rule, Show formula)
          => Parser (Command ix rule formula)
          -> ProverState ix rule formula
          -> IO (ProverState ix rule formula)
runProver pCommand initialState =
  execStateT (runInputT defaultSettings startRepl) initialState
 where
   startRepl = outputStrLn "Matching Logic prover started" >> repl
   repl = do
     mcommand <- getInputLine ">>> "
     case mcommand of
       Just command -> case parse pCommand "<stdin>" command of
         Left err -> outputStrLn (show err) >> repl
         Right cmd -> do
           outputStrLn (show cmd)
           ProverState state <- lift get
           case applyCommand cmd state of
             Just state' -> do
               lift (put (ProverState state'))
               outputStrLn (renderProof state')
               repl
             Nothing -> outputStrLn "command failed" >> repl
       Nothing -> return ()
