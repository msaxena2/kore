{-# LANGUAGE OverloadedStrings #-}
module Kore.MatchingLogic.ProofSystem.Minimal.Syntax(parseId, parseMLDerivation) where
import           Control.Applicative                    (some)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.State.Strict             (MonadState (..),
                                                         StateT, runStateT)
import           Data.Char                              (isAlphaNum, isSpace)
import           Data.List                              (isPrefixOf, isSuffixOf)
import           Data.Text                              (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- import           Kore.MatchingLogic.ProofSystem.Dummy
import           Kore.MatchingLogic.HilbertProof
import           Kore.MatchingLogic.ProofSystem.Minimal
-- import           Kore.MatchingLogic.ProverRepl

import           Data.Functor.Foldable                  (Fix (Fix))

import qualified Kore.MatchingLogic.AST                 as AST
import           Kore.MatchingLogic.Signature
{-
import           Kore.MatchingLogic.MetaK            (Label (MetaLabel), MetaK,
                                                      MetaLabel (Symbol),
                                                      MetaSort (SortSort),
                                                      Sort (MetaSort))
 -}

import           Data.Text.Prettyprint.Doc              (Doc, Pretty (pretty),
                                                         sep, tupled, (<>))
import qualified Data.Text.Prettyprint.Doc              as Doc
import           Kore.MatchingLogic.AST.Syntax          (mlPattern)

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme p = p <* space

parens :: Parser a -> Parser a
parens = between (lexeme (char '(')) (lexeme (char ')'))

comma :: Parser ()
comma = char ',' >> space

number :: Parser Int
number = read <$> some digitChar

parsePathPos :: Parser [Int]
parsePathPos = sepBy number space1

--Todo: Remove these declarations in favor of Kore Parsers
parseId :: Parser Text
parseId = lexeme $ takeWhile1P Nothing isAlphaNum

{-
parseFormula :: Parser Pattern
parseFormula = lexeme $ do
  text <- takeWhile1P Nothing isAlphaNum
  return (Fix (AST.Variable (MetaSort SortSort) text))

parseSymbol :: Parser Symbol
parseSymbol =  lexeme $ MetaLabel <$> choice
    [ sym Symbol "#symbol"
    ]
  where
    sym con text = con <$ string text

parseVar :: Parser Text
parseVar = lexeme $ pack <$> some (satisfy (\c -> not (isSpace c) && (c /= ':') && (isAlphaNum c)))
-}

infixl 4 `arg`
arg l r = l <* comma <*> r

parseMLDerivation :: forall sig var ix . (AST.IsSignature sig)
                  => Parser (Sort sig)
                  -> Parser (Label sig)
                  -> Parser var
                  -> Parser ix
                  -> Parser (MLRuleSig sig var ix)
parseMLDerivation pSort pLabel pVar pIx =
        propositionalRules
    <|> propagationRules
    <|> modusPonens
    <|> generalization
    <|> varsubst
    <|> forall
    <|> necessitation
    <|> existence
    <|> singvar
  where
    parseFormula :: Parser (AST.SigPattern sig var)
    parseFormula = mlPattern pSort pLabel pVar

    rule :: Text -> Parser (MLRuleSig sig var ix) -> Parser (MLRuleSig sig var ix)
    rule name body = string name >> space >> parens body

    propositionalRules :: Parser (MLRuleSig sig var ix)
    propositionalRules = try $ do
      string "propositional"
      prop1 <|> prop2 <|> prop3

    prop1 = rule "1" $ Propositional1 <$> parseFormula `arg` parseFormula
    prop2 = rule "2" $ Propositional2 <$> parseFormula `arg` parseFormula `arg` parseFormula
    prop3 = rule "3" $ Propositional3 <$> parseFormula `arg` parseFormula

    modusPonens = rule "mp" $
      ModusPonens <$> pIx `arg` pIx
    generalization = rule "ug" $
      Generalization <$> pVar `arg` pIx
    varsubst = rule "varsubst" $
      VariableSubstitution <$> pVar `arg` pIx `arg` pVar
    forall = rule "forall" $
      Forall <$> pVar `arg` parseFormula `arg` parseFormula
    necessitation = rule "necessitation" $
      Necessitation <$> pLabel `arg` number `arg` pIx

    propagationRules = do
      string "propagate-"
      propagateOr <|> propagateExists

    propagateOr = rule "or" $
      PropagateOr <$> pLabel `arg` number `arg` parseFormula `arg` parseFormula
    propagateExists = rule "exists" $
      PropagateExists <$> pLabel `arg` number `arg` pVar `arg` parseFormula

    existence = rule "exists" $
      Existence <$> pVar
    singvar = rule "singvar" $
      Singvar <$> pVar `arg` parseFormula `arg` parsePathPos `arg` parsePathPos

instance (Pretty sort, Pretty label, Pretty var, Pretty term, Pretty hyp)
       => Pretty (MLRule sort label var term hyp) where
  pretty p = let
      rule :: Text -> [Doc ann] -> Doc ann
      rule prefix args = pretty prefix <> tupled args
    in case p of
    Propositional1 p1 p2 -> rule "propositional1" [pretty p1,pretty p2]
    Propositional2 p1 p2 p3 -> rule "propositional2" [pretty p1,pretty p2,pretty p3]
    Propositional3 p1 p2 -> rule "propositional3" [pretty p1, pretty p2]
    ModusPonens h1 h2 -> rule "mp" [pretty h1,pretty h2]
    Generalization v h -> rule "ug" [pretty v,pretty h]
    VariableSubstitution x h y -> rule "varsubst" [pretty x,pretty h,pretty y]
    Forall v p1 p2 -> rule "forall" [pretty v,pretty p1,pretty p2]
    Necessitation lbl pos h -> rule "necessitation" [pretty lbl,pretty pos,pretty h]
    PropagateOr lbl pos p1 p2 -> rule "propagate-or" [pretty lbl,pretty pos,pretty p1,pretty p2]
    PropagateExists lbl pos x p -> rule "propagate-exists" [pretty lbl,pretty pos,pretty x,pretty p]
    Existence x -> rule "exists" [pretty x]
    Singvar v p path1 path2 -> rule "singvar" [pretty v,pretty p,sep (map pretty path1),sep (map pretty path2)]
