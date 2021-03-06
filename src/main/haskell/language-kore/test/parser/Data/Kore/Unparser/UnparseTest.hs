module Data.Kore.Unparser.UnparseTest ( unparseParseTests
                                      , unparseUnitTests
                                      ) where

import           Data.Kore.AST.Common
import           Data.Kore.AST.Kore
import           Data.Kore.AST.MetaOrObject
import           Data.Kore.ASTGen
import           Data.Kore.Parser.LexemeImpl
import           Data.Kore.Parser.ParserImpl
import           Data.Kore.Parser.ParserUtils
import           Data.Kore.Unparser.Unparse

import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (assertEqual, testCase)
import           Test.Tasty.QuickCheck        (forAll, testProperty)
import qualified Text.Megaparsec.Char         as Parser

unparseUnitTests :: TestTree
unparseUnitTests =
    testGroup
        "Unparse unit tests"
        [ unparseTest
            (asSentence
                (SentenceSort
                    { sentenceSortName = Id "x"
                    , sentenceSortParameters = []
                    , sentenceSortAttributes = Attributes []
                    }
                :: KoreSentenceSort)
            )
            "sort x{}[]"
        , unparseTest
            Attributes
                { getAttributes =
                    [ asKorePattern (TopPattern Top
                        { topSort = SortVariableSort SortVariable
                            { getSortVariable = Id "#Fm" :: Id Meta }
                        })
                    , asKorePattern (InPattern In
                        { inOperandSort = SortActualSort SortActual
                            { sortActualName = Id "B" :: Id Object
                            , sortActualSorts = []
                            }
                        , inResultSort = SortActualSort SortActual
                            { sortActualName = Id "G" :: Id Object
                            , sortActualSorts = []
                            }
                        , inContainedChild =
                            asKorePattern $ VariablePattern Variable
                                { variableName = Id "T" :: Id Object
                                , variableSort = SortVariableSort SortVariable
                                    { getSortVariable = Id {getId = "C"} }
                                }
                        , inContainingChild = asKorePattern (StringLiteralPattern
                            StringLiteral { getStringLiteral = "" })
                        })
                    ]
                }
            "[\n\
            \    \\top{#Fm}(),\n\
            \    \\in{\n\
            \        B{},\n\
            \        G{}\n\
            \    }(\n\
            \        T:C,\n\
            \        \"\"\n\
            \    )\n\
            \]"
        , unparseTest
            (Module
                { moduleName = ModuleName "t"
                , moduleSentences = []
                , moduleAttributes = Attributes []
                }::KoreModule
            )
            "module t\nendmodule\n[]"
        , unparseParseTest
            koreDefinitionParser
            Definition
                { definitionAttributes = Attributes {getAttributes = []}
                , definitionModules =
                    [ Module
                        { moduleName = ModuleName {getModuleName = "i"}
                        , moduleSentences = []
                        , moduleAttributes = Attributes {getAttributes = []}
                        }
                    , Module
                        { moduleName = ModuleName {getModuleName = "k"}
                        , moduleSentences = []
                        , moduleAttributes = Attributes {getAttributes = []}
                        }
                    ]
                }
        , unparseTest
            (Definition
                { definitionAttributes = Attributes {getAttributes = []}
                , definitionModules =
                    [ Module
                        { moduleName = ModuleName {getModuleName = "i"}
                        , moduleSentences = []
                        , moduleAttributes = Attributes {getAttributes = []}
                        }
                    , Module
                        { moduleName = ModuleName {getModuleName = "k"}
                        , moduleSentences = []
                        , moduleAttributes = Attributes {getAttributes = []}
                        }
                    ]
                }::KoreDefinition
            )
            (  "[]\n\n"
            ++ "    module i\n    endmodule\n    []\n"
            ++ "    module k\n    endmodule\n    []\n"
            )
        , unparseTest
            ( asSentence SentenceImport
                { sentenceImportModuleName = ModuleName {getModuleName = "sl"}
                , sentenceImportAttributes =
                    Attributes { getAttributes = [] } :: KoreAttributes
                }
            )
            "import sl[]"
        , unparseTest
            (Attributes
                { getAttributes =
                    [ asKorePattern
                        ( TopPattern Top
                            { topSort = SortActualSort SortActual
                                { sortActualName = Id "#CharList" :: Id Meta
                                , sortActualSorts = []
                                }
                            }
                        )
                    ]
                }::KoreAttributes
            )
        "[\n    \\top{#CharList{}}()\n]"
        , unparseTest
            (Attributes
                { getAttributes =
                    [ asKorePattern
                        (CharLiteralPattern CharLiteral
                            { getCharLiteral = '\'' }
                        )
                    , asKorePattern
                        (CharLiteralPattern CharLiteral
                            { getCharLiteral = '\'' }
                        )
                    ]
                }::KoreAttributes
            )
            "[\n    '\\'',\n    '\\''\n]"
        ]

unparseParseTests :: TestTree
unparseParseTests =
    testGroup
        "QuickCheck Unparse&Parse Tests"
        [ testProperty "Object Id"
            (forAll (idGen Object) (unparseParseProp (idParser Object)))
        , testProperty "Meta Id"
            (forAll (idGen Meta) (unparseParseProp (idParser Meta)))
        , testProperty "StringLiteral"
            (forAll stringLiteralGen (unparseParseProp stringLiteralParser))
        , testProperty "CharLiteral"
            (forAll charLiteralGen (unparseParseProp charLiteralParser))
        , testProperty "Object Symbol"
            (forAll (symbolGen Object) (unparseParseProp (symbolParser Object)))
        , testProperty "Meta Symbol"
            (forAll (symbolGen Meta) (unparseParseProp (symbolParser Meta)))
        , testProperty "Object Alias"
            (forAll (aliasGen Object) (unparseParseProp (aliasParser Object)))
        , testProperty "Meta Alias"
            (forAll (aliasGen Meta) (unparseParseProp (aliasParser Meta)))
        , testProperty "Object SortVariable"
            (forAll (sortVariableGen Object)
                (unparseParseProp (sortVariableParser Object))
            )
        , testProperty "Meta SortVariable"
            (forAll (sortVariableGen Meta)
                (unparseParseProp (sortVariableParser Meta))
            )
        , testProperty "Object Sort"
            (forAll (sortGen Object)
                (unparseParseProp (sortParser Object))
            )
        , testProperty "Meta Sort"
            (forAll (sortGen Meta)
                (unparseParseProp (sortParser Meta))
            )
        {-
        , testProperty "UnifiedVariable"
            (forAll unifiedVariableGen (unparseParseProp unifiedVariableParser))
        -}
        , testProperty "CommonKorePattern"
            (forAll korePatternGen (unparseParseProp korePatternParser))
        , testProperty "Attributes"
            (forAll
                (attributesGen korePatternGen)
                (unparseParseProp (attributesParser korePatternParser))
            )
        , testProperty "Sentence"
            (forAll koreSentenceGen (unparseParseProp koreSentenceParser))
        , testProperty "Module"
            (forAll
                (moduleGen koreSentenceGen korePatternGen)
                (unparseParseProp
                    (moduleParser koreSentenceParser korePatternParser)
                )
            )
        , testProperty "Definition"
            (forAll
                (definitionGen koreSentenceGen korePatternGen)
                (unparseParseProp
                    (definitionParser koreSentenceParser korePatternParser)
                )
            )
        ]

parse :: Parser a -> String -> Either String a
parse parser =
    parseOnly (parser <* endOfInput) "<test-string>"

unparseParseProp :: (Unparse a, Eq a) => Parser a -> a -> Bool
unparseParseProp p a = parse p (unparseToString a) == Right a

unparseParseTest
    :: (Unparse a, Eq a, Show a) => Parser a -> a -> TestTree
unparseParseTest parser astInput =
    testCase
        "Parsing + unparsing."
        (assertEqual "Expecting unparse success!"
            (Right astInput)
            (parse parser (unparseToString astInput)))

unparseTest :: (Unparse a, Show a) => a -> String -> TestTree
unparseTest astInput expected =
    testCase
        "Unparsing"
        (assertEqual ("Expecting unparse success!" ++ show astInput)
            expected
            (unparseToString astInput))
