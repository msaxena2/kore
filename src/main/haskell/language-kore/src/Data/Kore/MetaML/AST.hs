{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module      : Data.Kore.MetaML.AST
Description : Data Structures for representing a Meta-only version of the
              Kore language AST
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

This module specializes the 'Data.Kore.AST.Common' datastructures for
representing definitions, modules, axioms, patterns that only use 'Meta'-level
constructs.

Please refer to Section 9 (The Kore Language) of the
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf Semantics of K>.
-}
module Data.Kore.MetaML.AST where

import           Data.Kore.AST.Common
import           Data.Kore.AST.MetaOrObject
import           Data.Kore.AST.PureML
import           Data.Kore.Variables.Free   (pureFreeVariables)

import           Data.Set                   (Set)

{-|'MetaMLPattern' corresponds to "fixed point" representations
of the 'Pattern' class where the level is fixed to 'Meta'.

'var' is the type of variables.
-}
type MetaMLPattern variable = PureMLPattern Meta variable

-- |'MetaAttributes' is the 'Meta'-only version of 'Attributes'
type MetaAttributes = PureAttributes Meta

-- |'MetaSentenceAxiom' is the 'Meta'-only version of 'SentenceAxiom'
type MetaSentenceAxiom = PureSentenceAxiom Meta
-- |'MetaSentenceAlias' is the 'Meta'-only version of 'SentenceAlias'
type MetaSentenceAlias = PureSentenceAlias Meta
-- |'MetaSentenceSymbol' is the 'Meta'-only version of 'SentenceSymbol'
type MetaSentenceSymbol = PureSentenceSymbol Meta
-- |'MetaSentenceImport' is the 'Meta'-only version of 'SentenceImport'
type MetaSentenceImport = PureSentenceImport Meta
-- |'MetaSentenceImport' is the 'Meta'-only version of 'SentenceImport'
type MetaSentenceSort = PureSentenceSort Meta

-- |'MetaSentence' is the 'Meta'-only version of 'Sentence'
type MetaSentence = PureSentence Meta

instance AsSentence MetaSentence MetaSentenceImport where
    asSentence = SentenceImportSentence

instance AsSentence MetaSentence MetaSentenceAxiom where
    asSentence = SentenceAxiomSentence


-- |'MetaModule' is the 'Meta'-only version of 'Module'.
type MetaModule = PureModule Meta

-- |'MetaDefinition' is the 'Meta'-only version of 'Definition'.
type MetaDefinition = PureDefinition Meta

-- |'CommonMetaPattern' is the instantiation of 'MetaPattern' with common
-- 'Variable's.
type CommonMetaPattern = MetaMLPattern Variable
type PatternMetaType = Pattern Meta Variable CommonMetaPattern

type MetaPatternStub = PatternStub Meta Variable CommonMetaPattern

-- |'metaFreeVariables' collects the free variables of a 'CommonMetaPattern'.
metaFreeVariables :: CommonMetaPattern -> Set (Variable Meta)
metaFreeVariables = pureFreeVariables Meta

nilSortListHead :: SymbolOrAlias Meta
nilSortListHead = groundHead "#nilSortList"

consSortListHead :: SymbolOrAlias Meta
consSortListHead = groundHead "#consSortList"

nilSortListMetaPattern :: MetaMLPattern v
nilSortListMetaPattern = asPurePattern $ constant nilSortListHead

nilPatternListHead :: SymbolOrAlias Meta
nilPatternListHead = groundHead "#nilPatternList"

consPatternListHead :: SymbolOrAlias Meta
consPatternListHead = groundHead "#consPatternList"

nilPatternListMetaPattern :: MetaMLPattern v
nilPatternListMetaPattern = asPurePattern $ constant nilPatternListHead

variableHead :: SymbolOrAlias Meta
variableHead = groundHead "#variable"

variableAsPatternHead :: SymbolOrAlias Meta
variableAsPatternHead = groundHead "#variableAsPattern"

metaMLPatternHead :: MLPatternType -> SymbolOrAlias Meta
metaMLPatternHead pt = groundHead ('#' : '\\' : patternString pt)

sortDeclaredHead :: Sort Meta -> SymbolOrAlias Meta
sortDeclaredHead param = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#sortDeclared"
    , symbolOrAliasParams = [param]
    }

provableHead :: Sort Meta -> SymbolOrAlias Meta
provableHead param = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#provable"
    , symbolOrAliasParams = [param]
    }

sortsDeclaredHead :: Sort Meta -> SymbolOrAlias Meta
sortsDeclaredHead param = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#sortsDeclared"
    , symbolOrAliasParams = [param]
    }

symbolDeclaredHead :: Sort Meta -> SymbolOrAlias Meta
symbolDeclaredHead param = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#symbolDeclared"
    , symbolOrAliasParams = [param]
    }

sortHead :: SymbolOrAlias Meta
sortHead = groundHead "#sort"

symbolHead :: SymbolOrAlias Meta
symbolHead = groundHead "#symbol"

applicationHead :: SymbolOrAlias Meta
applicationHead = groundHead "#application"
