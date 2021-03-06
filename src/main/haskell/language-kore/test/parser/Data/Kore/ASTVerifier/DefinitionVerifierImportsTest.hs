module Data.Kore.ASTVerifier.DefinitionVerifierImportsTest
    (definitionVerifierImportsTests) where

import           Test.Tasty                                          (TestTree,
                                                                      testGroup)

import           Data.Kore.AST.Common
import           Data.Kore.AST.Kore
import           Data.Kore.AST.MetaOrObject
import           Data.Kore.ASTVerifier.DefinitionVerifierTestHelpers
import           Data.Kore.Error
import           Data.Kore.Implicit.ImplicitSorts

definitionVerifierImportsTests :: TestTree
definitionVerifierImportsTests =
    testGroup
        "Definition verifier - imports - unit tests"
        [ importTests
        , nameVisibilityTests
        , nameDuplicationTests
        ]

importTests :: TestTree
importTests =
    testGroup
        "Module imports"
        [ expectSuccess "Simplest definition"
            Definition
                { definitionAttributes = Attributes []
                , definitionModules =
                    [ Module
                        { moduleName = ModuleName "M1"
                        , moduleSentences = []
                        , moduleAttributes = Attributes []
                        }
                    ]
                }
        , expectSuccess "Two modules"
            Definition
                { definitionAttributes = Attributes []
                , definitionModules =
                    [ Module
                        { moduleName = ModuleName "M1"
                        , moduleSentences = []
                        , moduleAttributes = Attributes []
                        }
                    , Module
                        { moduleName = ModuleName "M2"
                        , moduleSentences = []
                        , moduleAttributes = Attributes []
                        }
                    ]
                }
        , expectSuccess "Two modules with import"
            Definition
                { definitionAttributes = Attributes []
                , definitionModules =
                    [ Module
                        { moduleName = ModuleName "M1"
                        , moduleSentences = [ importSentence (ModuleName "M2") ]
                        , moduleAttributes = Attributes []
                        }
                    , Module
                        { moduleName = ModuleName "M2"
                        , moduleSentences = []
                        , moduleAttributes = Attributes []
                        }
                    ]
                }
        , expectSuccess "Three modules with chain import"
            Definition
                { definitionAttributes = Attributes []
                , definitionModules =
                    [ Module
                        { moduleName = ModuleName "M1"
                        , moduleSentences = [ importSentence (ModuleName "M2") ]
                        , moduleAttributes = Attributes []
                        }
                    , Module
                        { moduleName = ModuleName "M2"
                        , moduleSentences = [ importSentence (ModuleName "M3") ]
                        , moduleAttributes = Attributes []
                        }
                    , Module
                        { moduleName = ModuleName "M3"
                        , moduleSentences = []
                        , moduleAttributes = Attributes []
                        }
                    ]
                }
        , expectSuccess "Three modules with dag import"
            Definition
                { definitionAttributes = Attributes []
                , definitionModules =
                    [ Module
                        { moduleName = ModuleName "M1"
                        , moduleSentences =
                            [ importSentence (ModuleName "M2")
                            , importSentence (ModuleName "M3")
                            ]
                        , moduleAttributes = Attributes []
                        }
                    , Module
                        { moduleName = ModuleName "M2"
                        , moduleSentences = [ importSentence (ModuleName "M3") ]
                        , moduleAttributes = Attributes []
                        }
                    , Module
                        { moduleName = ModuleName "M3"
                        , moduleSentences = []
                        , moduleAttributes = Attributes []
                        }
                    ]
                }
        , expectFailureWithError "Circular import"
            (Error
                [ "module 'M1'", "module 'M2'", "module 'M3'", "module 'M2'" ]
                "Circular module import dependency."
            )
            Definition
                { definitionAttributes = Attributes []
                , definitionModules =
                    [ Module
                        { moduleName = ModuleName "M1"
                        , moduleSentences = [ importSentence (ModuleName "M2") ]
                        , moduleAttributes = Attributes []
                        }
                    , Module
                        { moduleName = ModuleName "M2"
                        , moduleSentences = [ importSentence (ModuleName "M3") ]
                        , moduleAttributes = Attributes []
                        }
                    , Module
                        { moduleName = ModuleName "M3"
                        , moduleSentences = [ importSentence (ModuleName "M2") ]
                        , moduleAttributes = Attributes []
                        }
                    ]
                }
        ]


nameVisibilityTests :: TestTree
nameVisibilityTests =
    testGroup
        "Name visibility though module imports"
        (  sortVisibilityTests
        ++ symbolVisibilityTests
        ++ aliasVisibilityTests
        )


sortVisibilityTests :: [TestTree]
sortVisibilityTests =
    [ nameReferenceTests
        "Sort visibility in sorts"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "axiom declaration"
            , "\\top"
            , "sort 'sort2'"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInSortSentence)
        (SupportingSentences sortReferenceInSortSupportingSentences)
    , nameReferenceTests
        "Sort visibility in top pattern"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "axiom declaration"
            , "\\top"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInTopPatternSentence)
        (SupportingSentences [])
    , nameReferenceTests
        "Sort visibility in exists pattern"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "axiom declaration"
            , "\\exists 'var'"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInExistsPatternSentence)
        (SupportingSentences [])
    , nameReferenceTests
        "Sort visibility in and pattern"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "axiom declaration"
            , "\\and"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInAndPatternSentence)
        (SupportingSentences [])
    , nameReferenceTests
        "Sort visibility in next pattern"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "axiom declaration"
            , "\\next"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInNextPatternSentence)
        (SupportingSentences [])
    , nameReferenceTests
        "Sort visibility in pattern in pattern"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "axiom declaration"
            , "\\next"
            , "\\equals"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInPatternInPatternSentence)
        (SupportingSentences
            sortReferenceInPatternInPatternSupportingSentences)
    , nameReferenceTests
        "Sort visibility in symbol declaration - return sort"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "symbol 'symbol1' declaration"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInSentenceSymbolResultSortSentence)
        (SupportingSentences [])
    , nameReferenceTests
        "Sort visibility in symbol declaration - operand sort"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "symbol 'symbol1' declaration"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInSentenceSymbolSortsSentence)
        (SupportingSentences
            sortReferenceInSentenceSymbolSortsSupportSentences)
    , nameReferenceTests
        "Sort visibility in alias declaration - return sort"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "alias 'alias1' declaration"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInSentenceAliasResultSortSentence)
        (SupportingSentences [])
    , nameReferenceTests
        "Sort visibility in alias declaration - operand sort"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "alias 'alias1' declaration"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInSentenceAliasSortsSentence)
        (SupportingSentences
            sortReferenceInSentenceAliasSortsSupportSentences)
    , nameReferenceTests
        "Sort visibility in application patterns"
        (ExpectedErrorMessage "Sort 'sort1' not declared.")
        (ErrorStack
            [ "axiom declaration"
            , "symbol or alias 'symbol2'"
            , "sort 'sort1'"
            ]
        )
        (DeclaringSentence sortDeclaration)
        (UsingSentence sortReferenceInSymbolOrAliasSentence)
        (SupportingSentences sortReferenceInSymbolOrAliasSupportSentences)
    , testGroup
        "Meta sort visibility in top pattern"
        (nameReferenceSuccessTests
            -- N.B., this is not related to the used sort.
            (DeclaringSentence sortDeclaration)
            (UsingSentence metaSortReferenceInTopPatternSentence)
            (SupportingSentences [])
        )
    ]
  where
    sort = SortActualSort SortActual
        { sortActualName = Id "sort1"
        , sortActualSorts = []
        } :: Sort Object
    sortDeclaration = asSentence
        (SentenceSort
            { sentenceSortName = Id "sort1"
            , sentenceSortParameters = []
            , sentenceSortAttributes = Attributes []
            }
        ::KoreSentenceSort)
    anotherSort = SortActualSort SortActual
        { sortActualName = Id "sort3"
        , sortActualSorts = []
        } :: Sort Object
    anotherSortDeclaration = asSentence
        (SentenceSort
            { sentenceSortName = Id "sort3"
            , sentenceSortParameters = []
            , sentenceSortAttributes = Attributes []
            }
        :: KoreSentenceSort)
    topSortPattern = asKorePattern ( TopPattern Top { topSort = sort } )
    metaTopSortPattern =
        asKorePattern ( TopPattern Top { topSort = charMetaSort } )
    sortReferenceInSort =
        SortActualSort SortActual
            { sortActualName = Id "sort2"
            , sortActualSorts = [ sort ]
            } :: Sort Object
    sortReferenceInSortSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( TopPattern Top { topSort = sortReferenceInSort } )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    sortReferenceInSortSupportingSentences =
        [ asSentence
            (SentenceSort
                { sentenceSortName = Id "sort2"
                , sentenceSortParameters = [SortVariable (Id "x")]
                , sentenceSortAttributes = Attributes []
                }
            :: KoreSentenceSort)
        ]
    sortReferenceInTopPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = topSortPattern
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    metaSortReferenceInTopPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = metaTopSortPattern
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    sortReferenceInExistsPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( ExistsPattern Exists
                        { existsSort = sort
                        , existsVariable = Variable
                            { variableName = Id "var"
                            , variableSort = sort
                            }
                        , existsChild = asKorePattern
                            ( VariablePattern Variable
                                { variableName = Id "var"
                                , variableSort = sort
                                }
                            )
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    sortReferenceInAndPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( AndPattern And
                        { andSort = sort
                        , andFirst = topSortPattern
                        , andSecond = topSortPattern
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    sortReferenceInNextPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( NextPattern Next
                        { nextSort = sort
                        , nextChild = topSortPattern
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    sortReferenceInPatternInPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( NextPattern Next
                        { nextSort = anotherSort
                        , nextChild = asKorePattern
                            ( EqualsPattern Equals
                                { equalsResultSort = anotherSort
                                , equalsOperandSort = sort
                                , equalsFirst = topSortPattern
                                , equalsSecond = topSortPattern
                                }
                            )
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    sortReferenceInPatternInPatternSupportingSentences =
        [ anotherSortDeclaration ]
    sortReferenceInSentenceSymbolResultSortSentence =
        asSentence
            (SentenceSymbol
                { sentenceSymbolSymbol = Symbol
                    { symbolConstructor = Id "symbol1"
                    , symbolParams = []
                    }
                , sentenceSymbolSorts = []
                , sentenceSymbolResultSort = sort
                , sentenceSymbolAttributes = Attributes []
                }
            :: KoreSentenceSymbol Object)
    sortReferenceInSentenceSymbolSortsSentence =
        asSentence
            (SentenceSymbol
                { sentenceSymbolSymbol = Symbol
                    { symbolConstructor = Id "symbol1"
                    , symbolParams = []
                    }
                , sentenceSymbolSorts = [sort]
                , sentenceSymbolResultSort = anotherSort
                , sentenceSymbolAttributes = Attributes []
                }
            :: KoreSentenceSymbol Object)
    sortReferenceInSentenceSymbolSortsSupportSentences =
        [ anotherSortDeclaration ]
    sortReferenceInSentenceAliasResultSortSentence =
        asSentence
            (SentenceAlias
                { sentenceAliasAlias = Alias
                    { aliasConstructor = Id "alias1"
                    , aliasParams = []
                    }
                , sentenceAliasSorts = []
                , sentenceAliasResultSort = sort
                , sentenceAliasAttributes = Attributes []
                }
            :: KoreSentenceAlias Object)
    sortReferenceInSentenceAliasSortsSentence =
        asSentence
            (SentenceAlias
                { sentenceAliasAlias = Alias
                    { aliasConstructor = Id "alias1"
                    , aliasParams = []
                    }
                , sentenceAliasSorts = [sort]
                , sentenceAliasResultSort = anotherSort
                , sentenceAliasAttributes = Attributes []
                }
            :: KoreSentenceAlias Object)
    sortReferenceInSentenceAliasSortsSupportSentences =
        [ anotherSortDeclaration ]
    sortReferenceInSymbolOrAliasSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( ApplicationPattern Application
                        { applicationSymbolOrAlias = SymbolOrAlias
                            { symbolOrAliasConstructor = Id "symbol2"
                            , symbolOrAliasParams = [ sort ]
                            }
                        , applicationChildren = []
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    sortReferenceInSymbolOrAliasSupportSentences =
        [ asSentence
            (SentenceSymbol
                { sentenceSymbolSymbol = Symbol
                    { symbolConstructor = Id "symbol2"
                    , symbolParams = [SortVariable (Id "sv1")]
                    }
                , sentenceSymbolSorts = []
                , sentenceSymbolResultSort =
                    SortVariableSort (SortVariable (Id "sv1"))
                , sentenceSymbolAttributes = Attributes []
                }
            :: KoreSentenceSymbol Object)
        ]

symbolVisibilityTests :: [TestTree]
symbolVisibilityTests =
    [ nameReferenceTests
        "Symbol visibility in axioms"
        (ExpectedErrorMessage "Symbol 'symbol1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "symbol or alias 'symbol1'"
            ]
        )
        (DeclaringSentence symbolDeclaration)
        (UsingSentence symbolReferenceInAxiomSentence)
        (SupportingSentences defaultSymbolSupportSentences)
    , nameReferenceTests
        "Symbol visibility in attributes"
        (ExpectedErrorMessage "Symbol 'symbol1' not defined.")
        (ErrorStack
            [ "sort 'sort2' declaration"
            , "attributes"
            , "symbol or alias 'symbol1'"
            ]
        )
        (DeclaringSentence symbolDeclaration)
        (UsingSentence symbolReferenceInAttributesSentence)
        (SupportingSentences defaultSymbolSupportSentences)
    , nameReferenceTests
        "Symbol visibility in and pattern"
        (ExpectedErrorMessage "Symbol 'symbol1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "\\and"
            , "symbol or alias 'symbol1'"
            ]
        )
        (DeclaringSentence symbolDeclaration)
        (UsingSentence symbolReferenceInAndPatternSentence)
        (SupportingSentences defaultSymbolSupportSentences)
    , nameReferenceTests
        "Symbol visibility in exists pattern"
        (ExpectedErrorMessage "Symbol 'symbol1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "\\exists 'var'"
            , "symbol or alias 'symbol1'"
            ]
        )
        (DeclaringSentence symbolDeclaration)
        (UsingSentence symbolReferenceInExistsPatternSentence)
        (SupportingSentences defaultSymbolSupportSentences)
    , nameReferenceTests
        "Symbol visibility in next pattern"
        (ExpectedErrorMessage "Symbol 'symbol1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "\\next"
            , "symbol or alias 'symbol1'"
            ]
        )
        (DeclaringSentence symbolDeclaration)
        (UsingSentence symbolReferenceInNextPatternSentence)
        (SupportingSentences defaultSymbolSupportSentences)
    , nameReferenceTests
        "Symbol visibility in application pattern"
        (ExpectedErrorMessage "Symbol 'symbol1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "symbol or alias 'symbol2'"
            , "symbol or alias 'symbol1'"
            ]
        )
        (DeclaringSentence symbolDeclaration)
        (UsingSentence symbolReferenceInSymbolOrAliasSentence)
        (SupportingSentences symbolReferenceInSymbolOrAliasSupportSentences)
    , nameReferenceTests
        "Meta symbol visibility in axioms"
        (ExpectedErrorMessage "Symbol '#symbol1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "symbol or alias '#symbol1'"
            ]
        )
        (DeclaringSentence metaSymbolDeclaration)
        (UsingSentence metaSymbolReferenceInAxiomSentence)
        (SupportingSentences [])
    ]
  where
    topSortPattern = asKorePattern ( TopPattern Top { topSort = defaultSort } )
    symbolPattern :: CommonKorePattern
    symbolPattern = asKorePattern
        ( ApplicationPattern Application
            { applicationSymbolOrAlias = SymbolOrAlias
                { symbolOrAliasConstructor = Id "symbol1"
                , symbolOrAliasParams = [ defaultSort ]
                }
            , applicationChildren = []
            }
        )
    symbolDeclaration = asSentence
        (SentenceSymbol
            { sentenceSymbolSymbol = Symbol
                { symbolConstructor = Id "symbol1"
                , symbolParams = [SortVariable (Id "sv1")]
                }
            , sentenceSymbolSorts = []
            , sentenceSymbolResultSort =
                SortVariableSort (SortVariable (Id "sv1"))
            , sentenceSymbolAttributes = Attributes []
            }
        :: KoreSentenceSymbol Object)
    defaultSymbolSupportSentences = [ defaultSortDeclaration ]
    metaSymbolPattern = asKorePattern
        ( ApplicationPattern Application
            { applicationSymbolOrAlias = SymbolOrAlias
                { symbolOrAliasConstructor = Id "#symbol1"
                , symbolOrAliasParams = [ charMetaSort ]
                }
            , applicationChildren = []
            }
        )
    metaSymbolDeclaration = asSentence
        (SentenceSymbol
            { sentenceSymbolSymbol = Symbol
                { symbolConstructor = Id "#symbol1"
                , symbolParams = [SortVariable (Id "#sv1")]
                }
            , sentenceSymbolSorts = []
            , sentenceSymbolResultSort =
                SortVariableSort (SortVariable (Id "#sv1"))
            , sentenceSymbolAttributes = Attributes []
            }
        :: KoreSentenceSymbol Meta)
    symbolReferenceInAxiomSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = symbolPattern
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    metaSymbolReferenceInAxiomSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = metaSymbolPattern
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    symbolReferenceInAttributesSentence =
        asSentence
            (SentenceSort
                { sentenceSortName = Id "sort2"
                , sentenceSortParameters = []
                , sentenceSortAttributes = Attributes [symbolPattern]
                }
            :: KoreSentenceSort)
    symbolReferenceInAndPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( AndPattern And
                        { andSort = defaultSort
                        , andFirst = symbolPattern
                        , andSecond = topSortPattern
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    symbolReferenceInExistsPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( ExistsPattern Exists
                        { existsSort = defaultSort
                        , existsVariable = Variable
                            { variableName = Id "var"
                            , variableSort = defaultSort
                            }
                        , existsChild = symbolPattern
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    symbolReferenceInNextPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( NextPattern Next
                        { nextSort = defaultSort
                        , nextChild = symbolPattern
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    symbolReferenceInSymbolOrAliasSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( ApplicationPattern Application
                        { applicationSymbolOrAlias = SymbolOrAlias
                            { symbolOrAliasConstructor = Id "symbol2"
                            , symbolOrAliasParams = [ defaultSort ]
                            }
                        , applicationChildren = [symbolPattern]
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    symbolReferenceInSymbolOrAliasSupportSentences =
        asSentence
            (SentenceSymbol
                { sentenceSymbolSymbol = Symbol
                    { symbolConstructor = Id "symbol2"
                    , symbolParams = [SortVariable (Id "sv1")]
                    }
                , sentenceSymbolSorts =
                    [ SortVariableSort (SortVariable (Id "sv1")) ]
                , sentenceSymbolResultSort =
                    SortVariableSort (SortVariable (Id "sv1"))
                , sentenceSymbolAttributes = Attributes []
                }
            :: KoreSentenceSymbol Object)
        : defaultSymbolSupportSentences

aliasVisibilityTests :: [TestTree]
aliasVisibilityTests =
    [ nameReferenceTests
        "Alias visibility in axioms"
        (ExpectedErrorMessage "Symbol 'alias1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "symbol or alias 'alias1'"
            ]
        )
        (DeclaringSentence aliasDeclaration)
        (UsingSentence aliasReferenceInAxiomSentence)
        (SupportingSentences defaultAliasSupportSentences)
    , nameReferenceTests
        "Alias visibility in attributes"
        (ExpectedErrorMessage "Symbol 'alias1' not defined.")
        (ErrorStack
            [ "sort 'sort2' declaration"
            , "attributes"
            , "symbol or alias 'alias1'"
            ]
        )
        (DeclaringSentence aliasDeclaration)
        (UsingSentence aliasReferenceInAttributesSentence)
        (SupportingSentences defaultAliasSupportSentences)
    , nameReferenceTests
        "Alias visibility in and pattern"
        (ExpectedErrorMessage "Symbol 'alias1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "\\and"
            , "symbol or alias 'alias1'"
            ]
        )
        (DeclaringSentence aliasDeclaration)
        (UsingSentence aliasReferenceInAndPatternSentence)
        (SupportingSentences defaultAliasSupportSentences)
    , nameReferenceTests
        "Alias visibility in exists pattern"
        (ExpectedErrorMessage "Symbol 'alias1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "\\exists 'var'"
            , "symbol or alias 'alias1'"
            ]
        )
        (DeclaringSentence aliasDeclaration)
        (UsingSentence aliasReferenceInExistsPatternSentence)
        (SupportingSentences defaultAliasSupportSentences)
    , nameReferenceTests
        "Alias visibility in next pattern"
        (ExpectedErrorMessage "Symbol 'alias1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "\\next"
            , "symbol or alias 'alias1'"
            ]
        )
        (DeclaringSentence aliasDeclaration)
        (UsingSentence aliasReferenceInNextPatternSentence)
        (SupportingSentences defaultAliasSupportSentences)
    , nameReferenceTests
        "Alias visibility in application pattern"
        (ExpectedErrorMessage "Symbol 'alias1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "symbol or alias 'alias2'"
            , "symbol or alias 'alias1'"
            ]
        )
        (DeclaringSentence aliasDeclaration)
        (UsingSentence aliasReferenceInAliasOrAliasSentence)
        (SupportingSentences aliasReferenceInAliasOrAliasSupportSentences)
    , nameReferenceTests
        "Meta alias visibility in axioms"
        (ExpectedErrorMessage "Symbol '#alias1' not defined.")
        (ErrorStack
            [ "axiom declaration"
            , "symbol or alias '#alias1'"
            ]
        )
        (DeclaringSentence metaAliasDeclaration)
        (UsingSentence metaAliasReferenceInAxiomSentence)
        (SupportingSentences [])
    ]
  where
    topSortPattern = asKorePattern ( TopPattern Top { topSort = defaultSort } )
    aliasPattern :: CommonKorePattern
    aliasPattern = asKorePattern
        ( ApplicationPattern Application
            { applicationSymbolOrAlias = SymbolOrAlias
                { symbolOrAliasConstructor = Id "alias1"
                , symbolOrAliasParams = [ defaultSort ]
                }
            , applicationChildren = []
            }
        )
    aliasDeclaration = asSentence
        (SentenceAlias
            { sentenceAliasAlias = Alias
                { aliasConstructor = Id "alias1"
                , aliasParams = [SortVariable (Id "sv1")]
                }
            , sentenceAliasSorts = []
            , sentenceAliasResultSort =
                SortVariableSort (SortVariable (Id "sv1"))
            , sentenceAliasAttributes = Attributes []
            }
        :: KoreSentenceAlias Object)
    defaultAliasSupportSentences = [ defaultSortDeclaration ]
    metaAliasPattern = asKorePattern
        ( ApplicationPattern Application
            { applicationSymbolOrAlias = SymbolOrAlias
                { symbolOrAliasConstructor = Id "#alias1"
                , symbolOrAliasParams = [ charMetaSort ]
                }
            , applicationChildren = []
            }
        )
    metaAliasDeclaration = asSentence
        (SentenceAlias
            { sentenceAliasAlias = Alias
                { aliasConstructor = Id "#alias1"
                , aliasParams = [SortVariable (Id "#sv1")]
                }
            , sentenceAliasSorts = []
            , sentenceAliasResultSort =
                SortVariableSort (SortVariable (Id "#sv1"))
            , sentenceAliasAttributes = Attributes []
            }
        :: KoreSentenceAlias Meta)
    aliasReferenceInAxiomSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = aliasPattern
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    metaAliasReferenceInAxiomSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = metaAliasPattern
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    aliasReferenceInAttributesSentence =
        asSentence
            (SentenceSort
                { sentenceSortName = Id "sort2"
                , sentenceSortParameters = []
                , sentenceSortAttributes = Attributes [aliasPattern]
                }
            :: KoreSentenceSort)
    aliasReferenceInAndPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( AndPattern And
                        { andSort = defaultSort
                        , andFirst = aliasPattern
                        , andSecond = topSortPattern
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    aliasReferenceInExistsPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( ExistsPattern Exists
                        { existsSort = defaultSort
                        , existsVariable = Variable
                            { variableName = Id "var"
                            , variableSort = defaultSort
                            }
                        , existsChild = aliasPattern
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    aliasReferenceInNextPatternSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( NextPattern Next
                        { nextSort = defaultSort
                        , nextChild = aliasPattern
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    aliasReferenceInAliasOrAliasSentence =
        asSentence
            (SentenceAxiom
                { sentenceAxiomParameters = []
                , sentenceAxiomPattern = asKorePattern
                    ( ApplicationPattern Application
                        { applicationSymbolOrAlias = SymbolOrAlias
                            { symbolOrAliasConstructor = Id "alias2"
                            , symbolOrAliasParams = [ defaultSort ]
                            }
                        , applicationChildren = [aliasPattern]
                        }
                    )
                , sentenceAxiomAttributes = Attributes []
                }
            :: KoreSentenceAxiom)
    aliasReferenceInAliasOrAliasSupportSentences =
        (asSentence
            (SentenceAlias
                { sentenceAliasAlias = Alias
                    { aliasConstructor = Id "alias2"
                    , aliasParams = [SortVariable (Id "sv1")]
                    }
                , sentenceAliasSorts =
                    [ SortVariableSort (SortVariable (Id "sv1")) ]
                , sentenceAliasResultSort =
                    SortVariableSort (SortVariable (Id "sv1"))
                , sentenceAliasAttributes = Attributes []
                }
            :: KoreSentenceAlias Object)
        )
        : defaultAliasSupportSentences


defaultSort :: Sort Object
defaultSort = SortActualSort SortActual
    { sortActualName = Id "sort1"
    , sortActualSorts = []
    }
defaultSortDeclaration :: KoreSentence
defaultSortDeclaration = asSentence
    (SentenceSort
        { sentenceSortName = Id "sort1"
        , sentenceSortParameters = []
        , sentenceSortAttributes = Attributes []
        }
    :: KoreSentenceSort)

newtype DeclaringSentence = DeclaringSentence KoreSentence
newtype UsingSentence = UsingSentence KoreSentence
newtype SupportingSentences = SupportingSentences [KoreSentence]

nameReferenceTests
    :: String
    -> ExpectedErrorMessage
    -> ErrorStack
    -> DeclaringSentence
    -> UsingSentence
    -> SupportingSentences
    -> TestTree
nameReferenceTests
    description
    expectedErrorMessage
    additionalErrorStack
    declaringSentence
    usingSentence
    supportingSentences
  =
    testGroup description
        (  nameReferenceSuccessTests
            declaringSentence usingSentence supportingSentences
        ++ nameReferenceFailureTests
            expectedErrorMessage additionalErrorStack
            declaringSentence usingSentence supportingSentences
        )

nameReferenceSuccessTests
    :: DeclaringSentence
    -> UsingSentence
    -> SupportingSentences
    -> [TestTree]
nameReferenceSuccessTests
    (DeclaringSentence declaringSentence)
    (UsingSentence usingSentence)
    (SupportingSentences supportingSentences)
  =
    [ expectSuccess "Successful reference: one module"
        Definition
            { definitionAttributes = Attributes []
            , definitionModules =
                [ Module
                    { moduleName = ModuleName "M1"
                    , moduleSentences =
                        usingSentence
                        : declaringSentence
                        : supportingSentences
                    , moduleAttributes = Attributes []
                    }
                ]
            }
    , expectSuccess "Successful reference: two modules with import"
        Definition
            { definitionAttributes = Attributes []
            , definitionModules =
                [ Module
                    { moduleName = ModuleName "M1"
                    , moduleSentences =
                        [ importSentence (ModuleName "M2"), usingSentence]
                    , moduleAttributes = Attributes []
                    }
                , Module
                    { moduleName = ModuleName "M2"
                    , moduleSentences =
                        declaringSentence : supportingSentences
                    , moduleAttributes = Attributes []
                    }
                ]
            }
    , expectSuccess "Successful reference: three modules with chain import"
        Definition
            { definitionAttributes = Attributes []
            , definitionModules =
                [ Module
                    { moduleName = ModuleName "M1"
                    , moduleSentences =
                        [ importSentence (ModuleName "M2"), usingSentence]
                    , moduleAttributes = Attributes []
                    }
                , Module
                    { moduleName = ModuleName "M2"
                    , moduleSentences = [ importSentence (ModuleName "M3") ]
                    , moduleAttributes = Attributes []
                    }
                , Module
                    { moduleName = ModuleName "M3"
                    , moduleSentences =
                        declaringSentence : supportingSentences
                    , moduleAttributes = Attributes []
                    }
                ]
            }
    , expectSuccess "Successful reference: three modules with tree import"
        Definition
            { definitionAttributes = Attributes []
            , definitionModules =
                [ Module
                    { moduleName = ModuleName "M1"
                    , moduleSentences =
                        [ importSentence (ModuleName "M2")
                        , importSentence (ModuleName "M3")
                        , usingSentence
                        ]
                    , moduleAttributes = Attributes []
                    }
                , Module
                    { moduleName = ModuleName "M2"
                    , moduleSentences = []
                    , moduleAttributes = Attributes []
                    }
                , Module
                    { moduleName = ModuleName "M3"
                    , moduleSentences =
                        declaringSentence : supportingSentences
                    , moduleAttributes = Attributes []
                    }
                ]
            }
    , expectSuccess "Successful reference: three modules with dag import"
        Definition
            { definitionAttributes = Attributes []
            , definitionModules =
                [ Module
                    { moduleName = ModuleName "M1"
                    , moduleSentences =
                        [ importSentence (ModuleName "M2")
                        , importSentence (ModuleName "M3")
                        , usingSentence
                        ]
                    , moduleAttributes = Attributes []
                    }
                , Module
                    { moduleName = ModuleName "M2"
                    , moduleSentences = [ importSentence (ModuleName "M3") ]
                    , moduleAttributes = Attributes []
                    }
                , Module
                    { moduleName = ModuleName "M3"
                    , moduleSentences =
                        declaringSentence : supportingSentences
                    , moduleAttributes = Attributes []
                    }
                ]
            }
    ]

nameReferenceFailureTests
    :: ExpectedErrorMessage
    -> ErrorStack
    -> DeclaringSentence
    -> UsingSentence
    -> SupportingSentences
    -> [TestTree]
nameReferenceFailureTests
    (ExpectedErrorMessage expectedErrorMessage)
    (ErrorStack additionalErrorStack)
    (DeclaringSentence declaringSentence)
    (UsingSentence usingSentence)
    (SupportingSentences supportingSentences)
  =
    [ expectFailureWithError
        "Failed reference: One module without declaration"
        Error
            { errorContext = "module 'M1'" : additionalErrorStack
            , errorError = expectedErrorMessage
            }
        Definition
            { definitionAttributes = Attributes []
            , definitionModules =
                [ Module
                    { moduleName = ModuleName "M1"
                    , moduleSentences = usingSentence : supportingSentences
                    , moduleAttributes = Attributes []
                    }
                ]
            }
    , expectFailureWithError
        "Failed reference: two modules without import"
        Error
            { errorContext = "module 'M1'" : additionalErrorStack
            , errorError = expectedErrorMessage
            }
        Definition
            { definitionAttributes = Attributes []
            , definitionModules =
                [ Module
                    { moduleName = ModuleName "M1"
                    , moduleSentences = usingSentence : supportingSentences
                    , moduleAttributes = Attributes []
                    }
                , Module
                    { moduleName = ModuleName "M2"
                    , moduleSentences = [ declaringSentence ]
                    , moduleAttributes = Attributes []
                    }
                ]
            }
    , expectFailureWithError
        "Failed reference: two modules with reverse import"
        Error
            { errorContext = "module 'M2'" : additionalErrorStack
            , errorError = expectedErrorMessage
            }
        Definition
            { definitionAttributes = Attributes []
            , definitionModules =
                [ Module
                    { moduleName = ModuleName "M1"
                    , moduleSentences =
                        [ importSentence (ModuleName "M2")
                        , declaringSentence
                        ]
                    , moduleAttributes = Attributes []
                    }
                , Module
                    { moduleName = ModuleName "M2"
                    , moduleSentences = usingSentence : supportingSentences
                    , moduleAttributes = Attributes []
                    }
                ]
            }
    ]

nameDuplicationTests :: TestTree
nameDuplicationTests =
    testGroup
        "Name duplication accross modules"
        [ duplicatedNameFailureTest
            "Two sorts with the same name"
            "s1"
            (sortDeclarationModule (ModuleName "M1") (SortName "s1"))
            (sortDeclarationModule (ModuleName "M2") (SortName "s1"))
        , duplicatedNameFailureTest
            "Sort with the same name as symbol"
            "name"
            (sortDeclarationModule (ModuleName "M1") (SortName "name"))
            (symbolDeclarationModule (ModuleName "M2") (SymbolName "name"))
        , duplicatedNameFailureTest
            "Sort with the same name as symbol"
            "name"
            (symbolDeclarationModule (ModuleName "M1") (SymbolName "name"))
            (sortDeclarationModule (ModuleName "M2") (SortName "name"))
        , duplicatedNameFailureTest
            "Sort with the same name as alias"
            "name"
            (sortDeclarationModule (ModuleName "M1") (SortName "name"))
            (aliasDeclarationModule (ModuleName "M2") (AliasName "name"))
        , duplicatedNameFailureTest
            "Sort with the same name as alias"
            "name"
            (aliasDeclarationModule (ModuleName "M1") (AliasName "name"))
            (sortDeclarationModule (ModuleName "M2") (SortName "name"))

        , duplicatedNameFailureTest
            "Two symbols with the same name"
            "name"
            (symbolDeclarationModule (ModuleName "M1") (SymbolName "name"))
            (symbolDeclarationModule (ModuleName "M2") (SymbolName "name"))
        , duplicatedNameFailureTest
            "Symbol with the same name as alias"
            "name"
            (aliasDeclarationModule (ModuleName "M1") (AliasName "name"))
            (aliasDeclarationModule (ModuleName "M2") (AliasName "name"))
        , duplicatedNameFailureTest
            "Symbol with the same name as alias"
            "name"
            (aliasDeclarationModule (ModuleName "M1") (AliasName "name"))
            (symbolDeclarationModule (ModuleName "M2") (SymbolName "name"))

        , duplicatedNameFailureTest
            "Two aliases with the same name"
            "name"
            (aliasDeclarationModule (ModuleName "M1") (AliasName "name"))
            (aliasDeclarationModule (ModuleName "M2") (AliasName "name"))
        ]
  where
    sortDeclarationModule modName (SortName sortName) =
        Module
            { moduleName = modName
            , moduleSentences =
                [ asSentence
                    (SentenceSort
                        { sentenceSortName = Id sortName
                        , sentenceSortParameters = []
                        , sentenceSortAttributes = Attributes []
                        }
                    :: KoreSentenceSort)
                ]
            , moduleAttributes = Attributes []
            }
    symbolDeclarationModule modName (SymbolName symbolName) =
        Module
            { moduleName = modName
            , moduleSentences =
                [ asSentence
                    (SentenceSymbol
                        { sentenceSymbolSymbol = Symbol
                            { symbolConstructor = Id symbolName
                            , symbolParams = [SortVariable (Id "sv1")]
                            }
                        , sentenceSymbolSorts = []
                        , sentenceSymbolResultSort =
                            SortVariableSort (SortVariable (Id "sv1"))
                        , sentenceSymbolAttributes = Attributes []
                        }
                    :: KoreSentenceSymbol Object)
                ]
            , moduleAttributes = Attributes []
            }
    aliasDeclarationModule modName (AliasName aliasName) =
        Module
            { moduleName = modName
            , moduleSentences =
                [ asSentence
                    (SentenceAlias
                        { sentenceAliasAlias = Alias
                            { aliasConstructor = Id aliasName
                            , aliasParams = [SortVariable (Id "sv1")]
                            }
                        , sentenceAliasSorts = []
                        , sentenceAliasResultSort =
                            SortVariableSort (SortVariable (Id "sv1"))
                        , sentenceAliasAttributes = Attributes []
                        }
                    :: KoreSentenceAlias Object)
                ]
            , moduleAttributes = Attributes []
            }

duplicatedNameFailureTest
    :: String -> String -> KoreModule -> KoreModule -> TestTree
duplicatedNameFailureTest message duplicatedName module1 module2 =
    expectFailureWithError
        message
        Error
            { errorContext = ["module 'M2'"]
            , errorError = "Duplicated name: '" ++ duplicatedName ++ "'."
            }
        Definition
            { definitionAttributes = Attributes []
            , definitionModules =
                [ module1
                , module2
                ]
            }
