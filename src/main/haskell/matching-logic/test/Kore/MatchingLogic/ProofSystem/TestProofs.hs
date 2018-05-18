module Kore.MatchingLogic.ProofSystem.TestProofs where

import           Data.Kore.AST.Common
import           Test.Tasty                               (TestTree)
import           Test.Tasty.HUnit
import           Kore.MatchingLogic.ProofSystem.Minimal
import           Kore.MatchingLogic.HilbertProof          (Proof)
import           Data.Kore.AST.Builders
import           Data.Kore.AST.Kore
import           Data.Kore.AST.MetaOrObject
import           Data.Kore.AST.PureML
import           Data.Kore.ASTVerifier.AttributesVerifier
import           Data.Kore.ASTVerifier.DefinitionVerifier
import           Data.Kore.MetaML.Lift
import           Data.Kore.MetaML.MetaToKore
import           Data.Kore.ASTVerifier.DefinitionVerifier
import           Data.Kore.IndexedModule.IndexedModule   (IndexedModule)
import qualified Data.Map as Map
import           Data.Either                              (fromRight)
import           Data.Kore.IndexedModule.IndexedModule   (KoreIndexedModule)
import           Kore.MatchingLogic.ProofSystem.MLProofSystem
import           Data.Kore.MetaML.AST                     (CommonMetaPattern)
import           Data.Kore.Error
import           Kore.MatchingLogic.Error
-- Build a simple pattern using the builders

-- Simple Sort creator function.
-- Since the utility function/type is in language-kore test
-- it's replicated here.

-- TODO: Common way to share test resources

newtype SortName = SortName String

-- simpleSortAcutal :: (MetaOrObject level) => SortName -> SortActual 

simpleSortActual (SortName sort) =
    SortActual
        { sortActualName = Id sort
        , sortActualSorts = []
        }
simpleSort :: (MetaOrObject level) => SortName -> Sort level
simpleSort sortName =
    SortActualSort (simpleSortActual sortName)

natSort :: Sort Meta
natSort = simpleSort (SortName "Nat")

-- Symbol declarations begin with "s"
sZero :: PureSentenceSymbol Meta
sSucc :: PureSentenceSymbol Meta
sPlus :: PureSentenceSymbol Meta

sZero = symbol_ "zero" []                 natSort
sSucc = symbol_ "succ" [natSort]          natSort
sPlus = symbol_ "plus" [natSort, natSort] natSort


stringObjectVariable_ ::
  Sort Meta ->
  String    ->
  Variable Meta

stringObjectVariable_ sort name = Variable (Id name) sort

-- Helper Functions for sort nat
natObjectVariable_ :: String -> Variable Meta
natObjectVariable_  = stringObjectVariable_ natSort

natParameterizedVariable_ :: String -> CommonPurePatternStub Meta
natParameterizedVariable_ = parameterizedVariable_ natSort

-- For uniformity, we ensure that axioms are prefixed with a.

-- \exists T. zero = T
aFuncZero :: CommonPurePatternStub Meta
aFuncZero  = exists_
              (natObjectVariable_ "T")
              (equals_
                (applyS sZero [])
                (natParameterizedVariable_ "T")
              )
-- \forall X . \exists T . succ(X) = T
aAllSucc :: CommonPurePatternStub Meta
aAllSucc = forall_
            (natObjectVariable_ "X")
            (exists_
              (natObjectVariable_ "T")
              (equals_
                (applyS sSucc [natParameterizedVariable_ "X"])
                (natParameterizedVariable_ "T")
              )
            )

-- \forall X . \forall Y . \exists T . X + Y = T
aFuncPlus :: CommonPurePatternStub Meta
aFuncPlus = forall_
              (natObjectVariable_ "X")
              (forall_
                (natObjectVariable_ "Y")
                (exists_
                  (natObjectVariable_ "T")
                  (equals_
                    (applyS sPlus [ natParameterizedVariable_ "X"
                                   ,natParameterizedVariable_ "Y"]
                    )
                    (natParameterizedVariable_ "T")
                  )
                )
              )

-- \forall X . X + zero = x
aZeroIdentity :: CommonPurePatternStub Meta
aZeroIdentity = forall_
                  (natObjectVariable_ "X")
                  (equals_
                    (applyS sPlus [ applyS sZero []
                                   ,natParameterizedVariable_ "X"]
                    )
                    (natParameterizedVariable_ "X")
                  )


-- \forall X. \forall Y . X + succ(Y) = succ(X + Y)
aPlus :: CommonPurePatternStub Meta
aPlus = forall_
          (natObjectVariable_ "X")
          (forall_
            (natObjectVariable_ "Y")
            (equals_
              (applyS sPlus [
                              natParameterizedVariable_ "X"
                             ,(applyS sSucc [natParameterizedVariable_ "Y"])
                             ]
              )
              (applyS sSucc [
                              (applyS sPlus [  natParameterizedVariable_ "X"
                                             , natParameterizedVariable_ "Y"]
                              )
                            ]
              )
            )
          )

-- Fix the type of the definition to level Meta
type ObjectDefinition = PureDefinition Meta
type ObjectModule     = PureModule     Meta

-- The main module is nat.
-- We first lay out the axioms

natDecSentence = 
  SentenceSort {
                 sentenceSortName = Id "Nat" :: Id Meta
               , sentenceSortParameters = [] :: [SortVariable Meta]
               , sentenceSortAttributes = Attributes [] :: Attributes (Pattern Meta) Variable
  } 


declarations = (asSentence natDecSentence)
                : 
              (fmap asSentence
                [  sZero
                 , sSucc
                 , sPlus ])


mainNatModule :: PureModule Meta
mainNatModule =
  Module
    {   moduleName       = ModuleName "nat"
      , moduleSentences  = declarations
      , moduleAttributes = Attributes []
    }

mainNatDefinition :: PureDefinition Meta
mainNatDefinition = Definition (Attributes []) [mainNatModule]

mainKoreDefinition = definitionMetaToKore mainNatDefinition 



definitionVerificationCheck :: Assertion 
definitionVerificationCheck = 
  let x = (verifyAndIndexDefinition DoNotVerifyAttributes mainKoreDefinition)
   in case x of 
        Left (error) -> 
          assertFailure $ "Verification check failed: " ++ (printError error) 
        Right modulesMap ->
          print "Success" 


natFormulaVerifier :: CommonMetaPattern -> Either (Error MLError) ()
natFormulaVerifier = 
  case (verifyAndIndexDefinition DoNotVerifyAttributes mainKoreDefinition) of
    Left err         -> (\_ -> koreFail "Index Defintion Failure")
    Right modulesMap -> 
       (case (Map.lookup (ModuleName "Nat") modulesMap) of  
          Just mainModule -> (formulaVerifier mainModule)
          Nothing         -> (\_ -> koreFail "ModuleLookup Failure") 
        )

{-Instantiating types for Test proofs usage -} 
type MLHilbertRule = 
  MLRule 
    (Sort Meta) 
    (SymbolOrAlias Meta) 
    (Variable Meta)
    (CommonMetaPattern)

type MLHilbertProof = 
  Proof 
    Int 
    MLHilbertRule 
    CommonMetaPattern

-- emptyMLProof :: 
--   Hilbe 



onePlusOneProofTest :: TestTree
onePlusOneProofTest = testCaseSteps "1+1=2 ML Proof" $ \step -> do
  step "Index definition:" 
  definitionVerificationCheck 
  step "Add Axioms"
   -- axiomsAdditionTest

