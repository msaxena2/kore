{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-|
Module      : Data.Kore.AST.PureML
Description : Specifies the "pure" version of patterns, sentences, modules, and
              definition, which can be specialized to 'Object'-only and
              'Meta'-only objects.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

-}
module Data.Kore.AST.PureML where

import           Data.Fix
import           Data.Kore.AST.Common

{-|'PureMLPattern' corresponds to "fixed point" representations
of the 'Pattern' class where the level is fixed to a given @level@.

@var@ is the type of variables.
-}
type PureMLPattern level var = Fix (Pattern level var)

asPurePattern
    :: Pattern level var (PureMLPattern level var) -> PureMLPattern level var
asPurePattern = Fix

-- |'PureAttributes' is the pure (fixed-@level@) version of 'Attributes'
type PureAttributes level = Attributes (Pattern level) Variable

-- |'PureSentenceAxiom' is the pure (fixed-@level@) version of 'SentenceAxiom'
type PureSentenceAxiom level =
    SentenceAxiom (SortVariable level) (Pattern level) Variable
-- |'PureSentenceAlias' is the pure (fixed-@level@) version of 'SentenceAlias'
type PureSentenceAlias level =
    SentenceAlias level (Pattern level) Variable
-- |'PureSentenceSymbol' is the pure (fixed-@level@) version of 'SentenceSymbol'
type PureSentenceSymbol level =
    SentenceSymbol level (Pattern level) Variable
-- |'PureSentenceImport' is the pure (fixed-@level@) version of 'SentenceImport'
type PureSentenceImport level =
    SentenceImport (Pattern level) Variable
type PureSentenceSort level =
    SentenceSort level (Pattern level) Variable

-- |'PureSentence' is the pure (fixed-@level@) version of 'Sentence'
type PureSentence level =
    Sentence level (SortVariable level) (Pattern level) Variable

instance AsSentence (PureSentence level) (PureSentenceAlias level) where
    asSentence = SentenceAliasSentence

instance AsSentence (PureSentence level) (PureSentenceSymbol level) where
    asSentence = SentenceSymbolSentence

instance AsSentence (PureSentence level) (PureSentenceSort level) where
    asSentence = SentenceSortSentence

-- |'PureModule' is the pure (fixed-@level@) version of 'Module'
type PureModule level =
    Module (Sentence level) (SortVariable level) (Pattern level) Variable

-- |'PureDefinition' is the pure (fixed-@level@) version of 'Definition'
type PureDefinition level =
    Definition
        (Sentence level) (SortVariable level) (Pattern level) Variable

-- |Given a 'String', 'groundHead' produces the head of an 'Application'
-- corresponding to that argument.  @level@ is inferred from context.
groundHead :: String -> SymbolOrAlias level
groundHead ctor = SymbolOrAlias
    { symbolOrAliasConstructor = Id ctor
    , symbolOrAliasParams = []
    }

-- |Given a 'String', 'groundSymbol' produces the unparameterized 'Symbol'
-- corresponding to that argument.  @level@ is inferred from context.
groundSymbol :: String -> Symbol level
groundSymbol ctor = Symbol
    { symbolConstructor = Id ctor
    , symbolParams = []
    }

-- |Given a head and a list of children, produces an 'ApplicationPattern'
--  applying the given head to the children
apply :: SymbolOrAlias level -> [child] -> Pattern level variable child
apply patternHead patterns = ApplicationPattern Application
    { applicationSymbolOrAlias = patternHead
    , applicationChildren = patterns
    }

-- |Applies the given head to the empty list of children to obtain a
-- constant 'ApplicationPattern'
constant
    :: SymbolOrAlias level -> Pattern level variable child
constant patternHead = apply patternHead []

-- |'CommonPurePattern' is the instantiation of 'PureMLPattern' with common
-- 'Variable's.
type CommonPurePattern level = PureMLPattern level Variable
type UnFixedPureMLPattern level variable =
    Pattern level variable (PureMLPattern level variable)

type PurePatternStub level variable =
    PatternStub level variable (PureMLPattern level variable)

type CommonPurePatternStub level =
    PurePatternStub level Variable
