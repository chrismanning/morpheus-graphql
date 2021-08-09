{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Document
  ( gqlDocument,
    importGQLDocument,
    importGQLDocumentWithNamespace,
  )
where

import Data.ByteString.Lazy.Char8 (readFile)
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
  )
import Data.Morpheus.Server.TH.Compile
  ( compileDocument,
    gqlDocument,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString, readFile)

importGQLDocument :: FilePath -> Q [Dec]
importGQLDocument =
  importDeclarations CodeGenConfig {namespace = False}

importGQLDocumentWithNamespace :: FilePath -> Q [Dec]
importGQLDocumentWithNamespace =
  importDeclarations CodeGenConfig {namespace = True}

importDeclarations :: CodeGenConfig -> FilePath -> Q [Dec]
importDeclarations ctx src = do
  qAddDependentFile src
  runIO (readFile src)
    >>= compileDocument ctx
