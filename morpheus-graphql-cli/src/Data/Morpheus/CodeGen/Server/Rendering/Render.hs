{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.CodeGen.Server.Rendering.Render
  ( renderDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( ModuleDefinition (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.CodeGen.Server.Rendering.Terms
  ( renderExtension,
    renderImport,
  )
import Data.Morpheus.CodeGen.Server.Rendering.Type
  ( renderTypes,
  )
import Data.Text
  ( pack,
  )
import qualified Data.Text.Lazy as LT
  ( fromStrict,
  )
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    line,
    pretty,
    vsep,
  )

renderDocument :: String -> [ServerTypeDefinition] -> ByteString
renderDocument moduleName types =
  encodeUtf8
    $ LT.fromStrict
    $ pack
    $ show
    $ renderModuleDefinition
      ModuleDefinition
        { moduleName = pack moduleName,
          imports =
            [ ("Data.Data", ["Typeable"]),
              ("Data.Morpheus.Kind", ["TYPE"]),
              ("Data.Morpheus.Types", []),
              ("Data.Text", ["Text"]),
              ("GHC.Generics", ["Generic"]),
              ("Data.Map", ["fromList", "empty"])
            ],
          extensions =
            [ "DeriveAnyClass",
              "DeriveGeneric",
              "TypeFamilies",
              "OverloadedStrings"
            ],
          types
        }

renderModuleDefinition :: ModuleDefinition -> Doc n
renderModuleDefinition
  ModuleDefinition
    { extensions,
      moduleName,
      imports,
      types
    } =
    vsep (map renderExtension extensions)
      <> line
      <> line
      <> "module"
      <+> pretty moduleName
      <+> "where"
      <> line
      <> line
      <> vsep (map renderImport imports)
      <> line
      <> line
      <> either (error . show) id (renderTypes types)
