import           Test.Tasty                               (TestTree, testGroup)

import           Test.Tasty.Runners                       (consoleTestReporter, defaultMainWithIngredients,
                                                           listingTests)
import           Test.Tasty.Runners.AntXML                (antXMLRunner)

main :: IO ()
main = do
    inputFiles <- regressionTestsInputFiles "test/resources/"
    defaultMainWithIngredients
        [antXMLRunner, listingTests, consoleTestReporter]
        (allParserTests inputFiles)

allParserTests :: [String] -> TestTree
allParserTests regressionInputFiles =
    testGroup
        " All Matching Logic Tests"
        [ unitTests
        , regressionTests regressionInputFiles
        ]

unitTests :: TestTree
unitTests =
    testGroup
        " Unit Tests"
        [
        ]
