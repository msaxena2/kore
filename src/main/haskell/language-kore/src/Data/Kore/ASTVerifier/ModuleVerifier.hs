{-|
Module      : Data.Kore.ASTVerifier.ModuleVerifier
Description : Tools for verifying the wellformedness of a Kore 'Module'.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Data.Kore.ASTVerifier.ModuleVerifier ( verifyModule
                                            , verifyUniqueNames
                                            ) where

import           Data.Kore.AST.Common
import           Data.Kore.AST.Kore
import           Data.Kore.ASTVerifier.AttributesVerifier
import           Data.Kore.ASTVerifier.Error
import qualified Data.Kore.ASTVerifier.SentenceVerifier   as SentenceVerifier
import           Data.Kore.Error
import           Data.Kore.IndexedModule.IndexedModule

import qualified Data.Set                                 as Set

{-|'verifyUniqueNames' verifies that names defined in a module are unique both
within the module and outside, using the provided name set. -}
verifyUniqueNames
    :: Set.Set String
    -- ^ Names that are already defined.
    -> KoreModule
    -> Either (Error VerifyError) (Set.Set String)
    -- ^ On success returns the names that were previously defined together with
    -- the names defined in the given 'Module'.
verifyUniqueNames existingNames koreModule =
    withContext
        ("module '" ++ getModuleName (moduleName koreModule) ++ "'")
        (SentenceVerifier.verifyUniqueNames
            (moduleSentences koreModule)
            existingNames)

{-|'verifyModule' verifies the welformedness of a Kore 'Module'. -}
verifyModule
    :: AttributesVerification
    -> KoreIndexedModule
    -> Either (Error VerifyError) VerifySuccess
verifyModule attributesVerification indexedModule =
    withContext
        ("module '" ++ getModuleName (indexedModuleName indexedModule) ++ "'")
        (do
            verifyAttributes
                (indexedModuleAttributes indexedModule)
                indexedModule
                Set.empty
                attributesVerification
            SentenceVerifier.verifySentences
                indexedModule
                attributesVerification
                (indexedModuleRawSentences indexedModule)
        )
