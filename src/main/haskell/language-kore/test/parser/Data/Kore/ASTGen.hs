{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
module Data.Kore.ASTGen where

import           Test.QuickCheck.Gen         (Gen, choose, chooseAny, elements,
                                              frequency, getSize, listOf, oneof,
                                              scale, sized, suchThat, vectorOf)

import           Data.Fix
import           Data.Kore.AST.Common
import           Data.Kore.AST.Kore
import           Data.Kore.AST.MetaOrObject
import           Data.Kore.Parser.LexemeImpl

couple :: Gen a -> Gen [a]
couple gen = do
    size <- getSize
    if size <= 0
        then return []
        else choose (0,3) >>= (`vectorOf` gen)

couple1 :: Gen a -> Gen [a]
couple1 gen = do
    x <- gen
    xs <- couple gen
    return (x:xs)

{-# ANN genericIdGen "HLint: ignore Use String" #-}
genericIdGen :: [Char] -> [Char] -> Gen String
genericIdGen firstChars nextChars = do
    firstChar <- elements firstChars
    body <- listOf (elements nextChars)
    return (firstChar : body)

idGen :: MetaOrObject level => level -> Gen (Id level)
idGen x
    | isObject x = Id <$> objectId
    | otherwise  = Id . ('#' :) <$> objectId
  where
    objectId = genericIdGen idFirstChars (idFirstChars ++ idOtherChars)

stringLiteralGen :: Gen StringLiteral
stringLiteralGen = StringLiteral <$> listOf charGen

charLiteralGen :: Gen CharLiteral
charLiteralGen = CharLiteral <$> charGen

charGen :: Gen Char
charGen =
    suchThat
        (oneof
            [ chooseAny
            , elements "\a\b\f\n\r\t\v\\\"\'"
            , choose ('\32','\127')
            , choose ('\0','\255')
            , choose ('\0','\65535')
            ]
        )
        (/='?')

symbolOrAliasRawGen
    :: MetaOrObject level
    => level
    -> (Id level -> [Sort level] -> s level)
    -> Gen (s level)
symbolOrAliasRawGen x constructor = pure constructor
    <*> scale (`div` 2) (idGen x)
    <*> couple (scale (`div` 2) (sortGen x))

symbolOrAliasDeclarationRawGen
    :: MetaOrObject level
    => level
    -> (Id level -> [SortVariable level] -> s level)
    -> Gen (s level)
symbolOrAliasDeclarationRawGen x constructor = pure constructor
    <*> scale (`div` 2) (idGen x)
    <*> couple (scale (`div` 2) (sortVariableGen x))

symbolOrAliasGen :: MetaOrObject level => level -> Gen (SymbolOrAlias level)
symbolOrAliasGen x = symbolOrAliasRawGen x SymbolOrAlias

symbolGen :: MetaOrObject level => level -> Gen (Symbol level)
symbolGen x = symbolOrAliasDeclarationRawGen x Symbol

aliasGen :: MetaOrObject level => level -> Gen (Alias level)
aliasGen x = symbolOrAliasDeclarationRawGen x Alias

sortVariableGen :: MetaOrObject level => level -> Gen (SortVariable level)
sortVariableGen x = SortVariable <$> idGen x

sortActualGen :: MetaOrObject level => level -> Gen (SortActual level)
sortActualGen x
    | isObject x = pure SortActual
        <*> scale (`div` 2) (idGen x)
        <*> couple (scale (`div` 2) (sortGen x))
    | otherwise = SortActual <$>
        (Id <$> elements (map show metaSortsList)) <*> pure []

sortGen :: MetaOrObject level => level -> Gen (Sort level)
sortGen x = oneof
    [ SortVariableSort <$> sortVariableGen x
    , SortActualSort <$> sortActualGen x
    ]

unifiedSortVariableGen :: Gen UnifiedSortVariable
unifiedSortVariableGen = oneof
    [ UnifiedObject <$> sortVariableGen Object
    , UnifiedMeta <$> sortVariableGen Meta
    ]

moduleNameGen :: Gen ModuleName
moduleNameGen = ModuleName <$>
    genericIdGen idFirstChars (idFirstChars ++ idOtherChars)

variableGen :: MetaOrObject level => level -> Gen (Variable level)
variableGen x = pure Variable
    <*> scale (`div` 2) (idGen x)
    <*> scale (`div` 2) (sortGen x)

unifiedVariableGen :: Gen (Unified Variable)
unifiedVariableGen = scale (`div` 2) $ oneof
    [ UnifiedObject <$> variableGen Object
    , UnifiedMeta <$> variableGen Meta
    ]

unaryOperatorGen
    :: MetaOrObject level
    => Gen child
    -> level
    -> (Sort level -> child -> b level child)
    -> Gen (b level child)
unaryOperatorGen childGen x constructor = pure constructor
    <*> scale (`div` 2) (sortGen x)
    <*> scale (`div` 2) childGen

binaryOperatorGen
    :: MetaOrObject level
    => Gen child
    -> level
    -> (Sort level -> child -> child -> b level child)
    -> Gen (b level child)
binaryOperatorGen childGen x constructor = pure constructor
    <*> scale (`div` 2) (sortGen x)
    <*> scale (`div` 2) childGen
    <*> scale (`div` 2) childGen

ceilFloorGen
    :: MetaOrObject level
    => Gen child
    -> level
    -> (Sort level -> Sort level -> child -> c level child)
    -> Gen (c level child)
ceilFloorGen childGen x constructor = pure constructor
    <*> scale (`div` 2) (sortGen x)
    <*> scale (`div` 2) (sortGen x)
    <*> scale (`div` 2) childGen

equalsInGen
    :: MetaOrObject level
    => Gen child
    -> level
    -> (Sort level -> Sort level -> child -> child -> c level child)
    -> Gen (c level child)
equalsInGen childGen x constructor = pure constructor
    <*> scale (`div` 2) (sortGen x)
    <*> scale (`div` 2) (sortGen x)
    <*> scale (`div` 2) childGen
    <*> scale (`div` 2) childGen

existsForallGen
    :: MetaOrObject level
    => Gen child
    -> level
    -> (Sort level -> Variable level -> child -> q level Variable child)
    -> Gen (q level Variable child)
existsForallGen childGen x constructor = pure constructor
    <*> scale (`div` 2) (sortGen x)
    <*> scale (`div` 2) (variableGen x)
    <*> scale (`div` 2) childGen

topBottomGen
    :: MetaOrObject level
    => level
    -> (Sort level -> t level child)
    -> Gen (t level child)
topBottomGen x constructor = pure constructor
    <*> sortGen x

andGen :: MetaOrObject level => Gen child -> level -> Gen (And level child)
andGen childGen x = binaryOperatorGen childGen x And

applicationGen
    :: MetaOrObject level
    => Gen child
    -> level
    -> Gen (Application level child)
applicationGen childGen x = pure Application
    <*> scale (`div` 2) (symbolOrAliasGen x)
    <*> couple (scale (`div` 4) childGen)

bottomGen :: MetaOrObject level => level -> Gen (Bottom level child)
bottomGen x = topBottomGen x Bottom

ceilGen :: MetaOrObject level => Gen child -> level -> Gen (Ceil level child)
ceilGen childGen x = ceilFloorGen childGen x Ceil

equalsGen
    :: MetaOrObject level => Gen child -> level -> Gen (Equals level child)
equalsGen childGen x = equalsInGen childGen x Equals

domainValueGen
    :: MetaOrObject level => level -> Gen (DomainValue level CommonKorePattern)
domainValueGen x = pure DomainValue
    <*> scale (`div` 2) (sortGen x)
    <*> (asKorePattern . StringLiteralPattern <$> stringLiteralGen)

existsGen
    :: MetaOrObject level
    => Gen child
    -> level
    -> Gen (Exists level Variable child)
existsGen childGen x = existsForallGen childGen x Exists

floorGen :: MetaOrObject level => Gen child -> level -> Gen (Floor level child)
floorGen childGen x = ceilFloorGen childGen x Floor

forallGen
    :: MetaOrObject level
    => Gen child -> level -> Gen (Forall level Variable child)
forallGen childGen x = existsForallGen childGen x Forall

iffGen :: MetaOrObject level => Gen child -> level -> Gen (Iff level child)
iffGen childGen x = binaryOperatorGen childGen x Iff

impliesGen
    :: MetaOrObject level => Gen child -> level -> Gen (Implies level child)
impliesGen childGen x = binaryOperatorGen childGen x Implies

inGen :: MetaOrObject level => Gen child -> level -> Gen (In level child)
inGen childGen x = equalsInGen childGen x In

nextGen :: MetaOrObject level => Gen child -> level -> Gen (Next level child)
nextGen childGen x = unaryOperatorGen childGen x Next

notGen :: MetaOrObject level => Gen child -> level -> Gen (Not level child)
notGen childGen x = unaryOperatorGen childGen x Not

orGen :: MetaOrObject level => Gen child -> level -> Gen (Or level child)
orGen childGen x = binaryOperatorGen childGen x Or

rewritesGen
    :: MetaOrObject level => Gen child -> level -> Gen (Rewrites level child)
rewritesGen childGen x = binaryOperatorGen childGen x Rewrites

topGen :: MetaOrObject level => level -> Gen (Top level child)
topGen x = topBottomGen x Top


patternGen
    :: MetaOrObject level
    => Gen child
    -> level
    -> Gen (Pattern level Variable child)
patternGen childGen x =
    oneof
        [ AndPattern <$> andGen childGen x
        , ApplicationPattern <$> applicationGen childGen x
        , BottomPattern <$> bottomGen x
        , CeilPattern <$> ceilGen childGen x
        , EqualsPattern <$> equalsGen childGen x
        , ExistsPattern <$> existsGen childGen x
        , FloorPattern <$> floorGen childGen x
        , ForallPattern <$> forallGen childGen x
        , IffPattern <$> iffGen childGen x
        , ImpliesPattern <$> impliesGen childGen x
        , InPattern <$> inGen childGen x
        , NotPattern <$> notGen childGen x
        , OrPattern <$> orGen childGen x
        , TopPattern <$> topGen x
        , VariablePattern <$> variableGen x
        ]

korePatternGen :: Gen CommonKorePattern
korePatternGen = sized (\n ->
    if n<=0
        then oneof
            [ asKorePattern . StringLiteralPattern <$> stringLiteralGen
            , asKorePattern . CharLiteralPattern <$> charLiteralGen
            ]
        else frequency
            [ (15, asKorePattern <$> patternGen korePatternGen Meta)
            , (15, asKorePattern <$> patternGen korePatternGen Object)
            , (1, asKorePattern . StringLiteralPattern <$> stringLiteralGen)
            , (1, asKorePattern . CharLiteralPattern <$> charLiteralGen)
            , (1, asKorePattern . DomainValuePattern <$> domainValueGen Object)
            , (1, asKorePattern . NextPattern
                <$> nextGen korePatternGen Object)
            , (1, asKorePattern . RewritesPattern
                <$> rewritesGen korePatternGen Object)
            ]
    )

sentenceAliasGen
    :: MetaOrObject level
    => Gen (Fix (pat variable))
    -> level
    -> Gen (SentenceAlias level pat variable)
sentenceAliasGen patGen x = pure SentenceAlias
    <*> scale (`div` 2) (aliasGen x)
    <*> couple (scale (`div` 2) (sortGen x))
    <*> scale (`div` 2) (sortGen x)
    <*> scale (`div` 2) (attributesGen patGen)

sentenceSymbolGen
    :: MetaOrObject level
    => Gen (Fix (pat variable))
    -> level
    -> Gen (SentenceSymbol level pat variable)
sentenceSymbolGen patGen x = pure SentenceSymbol
    <*> scale (`div` 2) (symbolGen x)
    <*> couple (scale (`div` 2) (sortGen x))
    <*> scale (`div` 2) (sortGen x)
    <*> scale (`div` 2) (attributesGen patGen)

sentenceImportGen
    :: Gen (Fix (pat variable)) -> Gen (SentenceImport pat variable)
sentenceImportGen patGen = pure SentenceImport
    <*> scale (`div` 2) moduleNameGen
    <*> scale (`div` 2) (attributesGen patGen)

sentenceAxiomGen
   :: Gen sortParam
   -> Gen (Fix (pat var))
   -> Gen (SentenceAxiom sortParam pat var)
sentenceAxiomGen sortParamGen patGen =
    pure SentenceAxiom
        <*> couple (scale (`div` 2) sortParamGen)
        <*> scale (`div` 2) patGen
        <*> scale (`div` 2) (attributesGen patGen)

sentenceSortGen
    :: MetaOrObject level
    => Gen (Fix (pat var)) -> level -> Gen (SentenceSort level pat var)
sentenceSortGen patGen level =
    pure SentenceSort
        <*> scale (`div` 2) (idGen level)
        <*> couple (scale (`div` 2) (sortVariableGen level))
        <*> scale (`div` 2) (attributesGen patGen)

attributesGen :: Gen (Fix (pat variable)) -> Gen (Attributes pat variable)
attributesGen patGen = Attributes <$> couple (scale (`div` 4) patGen)

symbolOrAliasSentenceGen
    :: MetaOrObject level
    => Gen (Fix (pat variable))
    -> level
    -> Gen (Sentence level sortParam pat variable)
symbolOrAliasSentenceGen patGen level = oneof
    [ SentenceAliasSentence <$> sentenceAliasGen patGen level
    , SentenceSymbolSentence <$> sentenceSymbolGen patGen level
    ]

koreSentenceGen :: Gen KoreSentence
koreSentenceGen = oneof
    [ asSentence <$> sentenceAliasGen korePatternGen Meta
    , asSentence <$> sentenceSymbolGen korePatternGen Meta
    , asSentence <$> sentenceAliasGen korePatternGen Object
    , asSentence <$> sentenceSymbolGen korePatternGen Object
    , asSentence <$> sentenceImportGen korePatternGen
    , asSentence <$> sentenceAxiomGen unifiedSortVariableGen korePatternGen
    , asSentence <$> sentenceSortGen korePatternGen Object
    ]

moduleGen
    :: Gen (sentence sortParam pat variable)
    -> Gen (Fix (pat variable))
    -> Gen (Module sentence sortParam pat variable)
moduleGen senGen patGen = pure Module
    <*> scale (`div` 2) moduleNameGen
    <*> couple (scale (`div` 2) senGen)
    <*> scale (`div` 2) (attributesGen patGen)

modulesGen
    :: Gen (sentence sortParam pat variable)
    -> Gen (Fix (pat variable))
    -> Gen [Module sentence sortParam pat variable]
modulesGen senGen patGen = couple1 (scale (`div` 2) (moduleGen senGen patGen))

definitionGen
    :: Gen (sentence sortParam pat variable)
    -> Gen (Fix (pat variable))
    -> Gen (Definition sentence sortParam pat variable)
definitionGen senGen patGen = pure Definition
    <*> scale (`div` 2) (attributesGen patGen)
    <*> scale (`div` 2) (modulesGen senGen patGen)
