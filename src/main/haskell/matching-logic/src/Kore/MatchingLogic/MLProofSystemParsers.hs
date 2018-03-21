module Kore.MatchingLogic.MLProofSystemParsers(parseId, parseFormula, parseMLDerivation) where
import Data.Char(isSpace, isAlphaNum)
import Control.Applicative(some)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.State.Strict(StateT,runStateT,MonadState(..))
import Data.List(isPrefixOf,isSuffixOf)
import Text.Parsec
import Text.Parsec.String
import Data.Text


import Kore.MatchingLogic.ProverRepl
import Kore.MatchingLogic.HilbertProof
import Kore.MatchingLogic.DummyProofSystem
import Kore.MatchingLogic.MLProofSystem

--Todo: Remove these declarations in favor of Kore Parsers
parseId :: Parser Text
parseId = pack <$> some (satisfy (\c -> not (isSpace c) && (c /= ':') && (isAlphaNum c)))

parseFormula :: Parser Text
parseFormula =
  pack <$> some (satisfy (\c -> not (isSpace c) && (c /= ':') && (isAlphaNum c)))


parseVar :: Parser Text
parseVar = pack <$> some (satisfy (\c -> not (isSpace c) && (c /= ':') && (isAlphaNum c)))


parseMLDerivation :: Parser (MLRule, [Text])

propRules = do
    r <- propositionalRules <|> propagationRules
    return r

propositionalRules = do
    try $ string "propositional"
    rule <- (prop1 <|> prop2 <|> prop3)
    return rule

prop1 = do
    string "1"
    spaces
    char '('
    p1 <- parseFormula
    spaces
    char ','
    spaces
    p2 <- parseFormula
    spaces
    char ')'
    return (Propositional1 p1 p2, [])

prop2 = do
    string "2"
    spaces
    char '('
    p1 <- parseFormula
    spaces
    char ','
    spaces
    p2 <- parseFormula
    char ','
    spaces
    p3 <- parseFormula
    spaces
    char ')'
    return (Propositional2 p1 p2 p3, [])


prop3 = do
    string "3"
    spaces
    char '('
    p1 <- parseFormula
    spaces
    char ','
    spaces
    p2 <- parseFormula
    spaces
    char ')'
    return (Propositional3 p1 p2, [])

modusPonens = do
    string "mp"
    spaces
    char '('
    id1 <- parseId
    spaces
    char ','
    spaces
    id2 <- parseId
    spaces
    char ')'
    return (ModusPonens id1 id2, [])

generalization = do
    string "ug"
    spaces
    char '('
    v1 <- parseVar
    spaces
    char ','
    spaces
    id <- parseId
    spaces
    char ')'
    return (Generalization v1 id, [])

varsubst = do
    string "varsubst"
    spaces
    char '('
    v1 <- parseVar
    spaces
    char ','
    spaces
    id <- parseId
    spaces
    char ','
    v2 <- parseVar
    spaces
    char ')'
    return (VariableSubstitution v1 id v2, [])

forall = do
    string "forall"
    spaces
    char '('
    v1 <- parseVar
    spaces
    char ','
    spaces
    p1 <- parseFormula
    spaces
    char ','
    p2 <- parseFormula
    spaces
    char ')'
    return (Forall v1 p1 p2, [])

necessitation = do
    string "necessitation"
    spaces
    char '('
    symbol <- parseFormula
    spaces
    char ','
    spaces
    pos <- many1 digit
    spaces
    char ','
    id <- parseId
    spaces
    char ')'
    return (Necessitation symbol (read pos) id, [])


propagateOr = do
    string "or"
    spaces
    char '('
    symbol <- parseFormula
    spaces
    char ','
    spaces
    pos <- many1 digit
    spaces
    char ','
    p1 <- parseFormula
    spaces
    char ','
    spaces
    p2 <- parseFormula
    spaces
    char ')'
    return (PropagateOr symbol (read pos) p1 p2, [])

propagateExists = do
    string "exists"
    spaces
    char '('
    symbol <- parseFormula
    spaces
    char ','
    spaces
    pos <- many1 digit
    spaces
    char ','
    var <- parseVar
    spaces
    char ','
    spaces
    p1 <- parseFormula
    char ')'
    return (PropagateExists symbol (read pos) var p1, [])

propagationRules = do
    spaces
    string "propagate-"
    r <- propagateOr <|> propagateExists
    return r

existence = do
    spaces
    string "exists"
    spaces
    char '('
    spaces
    var <- parseVar
    spaces
    char ')'
    spaces
    return (Existence var, [])

parsePathPos :: Parser [Int]

parsePathPos = do
    cur <- many1 digit
    spaces
    rest <- option ([]) (parsePathPos)
    return $ [(read cur)] ++ rest

singvar = do
    spaces
    string "singvar"
    spaces
    char '('
    var <- parseVar
    spaces
    char ','
    spaces
    pat <- parseFormula
    spaces
    char ','
    spaces
    path1 <- parsePathPos
    char ','
    spaces
    path2 <- parsePathPos
    spaces
    char ')'
    return (Singvar var pat path1 path2, [])


parseMLDerivation = do
    spaces
    r <- propRules <|> modusPonens <|> generalization <|> varsubst <|> forall <|> necessitation <|> existence <|> singvar
    return r
