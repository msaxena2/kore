import           Test.Tasty                                        (TestTree,
                                                                    testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.Runners                                (consoleTestReporter,
                                                                    defaultMainWithIngredients,
                                                                    listingTests)
import           Test.Tasty.Runners.AntXML                         (antXMLRunner)

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Kore.MatchingLogic.ProofSystem.ProofAssistantTest
import           RuleParserTests
import           Kore.MatchingLogic.ProofSystem.TestProofs


main :: IO ()
main =
    defaultMainWithIngredients
        [antXMLRunner, listingTests, consoleTestReporter]
        allParserTests

allParserTests :: TestTree
allParserTests =
    testGroup
        " All Matching Logic Tests"
        [ unitTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        " Unit Tests"
        [ proofAssistantTests
        , parserUnitTests
        , onePlusOneProofTest
        ]


parserUnitTests :: TestTree
parserUnitTests = testGroup
                    " Rule Parser/Unparser Unit Tests"
                    [  ruleParsingTests
                      ,ruleUnparsingTests
                    ]
