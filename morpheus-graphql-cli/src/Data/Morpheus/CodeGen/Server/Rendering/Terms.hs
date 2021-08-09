{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.CodeGen.Server.Rendering.Terms
  ( renderExtension,
    renderWrapped,
    label,
    renderName,
    parametrizedType,
    TypeDoc (..),
    appendType,
    optional,
    renderImport,
  )
where

import Data.Morpheus.CodeGen.Server.Internal.AST
  ( TypeName,
    TypeWrapper (..),
    unpackName,
  )
import Data.Text
  ( Text,
  )
import Data.Text.Prettyprint.Doc
  ( (<+>),
    Doc,
    hsep,
    list,
    pretty,
    tupled,
  )

parametrizedType :: TypeName -> [TypeName] -> Doc ann
parametrizedType tName typeParameters = hsep $ map renderName (tName : typeParameters)

-- TODO: this should be done in transformer
renderName :: TypeName -> Doc ann
renderName "Boolean" = "Bool"
renderName "String" = "Text"
renderName name = pretty (unpackName name)

renderExtension :: Text -> Doc ann
renderExtension name = "{-#" <+> "LANGUAGE" <+> pretty name <+> "#-}"

data TypeDoc n = TypeDoc
  { isComplex :: Bool,
    unDoc :: Doc n
  }

appendType :: TypeName -> TypeDoc n -> TypeDoc n
appendType t1 TypeDoc {isComplex, unDoc = doc} = TypeDoc True $ renderName t1 <> " " <> if isComplex then tupled [doc] else doc

renderMaybe :: Bool -> TypeDoc n -> TypeDoc n
renderMaybe True = id
renderMaybe False = appendType "Maybe"

renderList :: TypeDoc n -> TypeDoc n
renderList = TypeDoc False . list . pure . unDoc

renderWrapped :: TypeWrapper -> TypeDoc n -> TypeDoc n
renderWrapped (TypeList wrapper notNull) = renderMaybe notNull . renderList . renderWrapped wrapper
renderWrapped (BaseType notNull) = renderMaybe notNull

label :: TypeName -> Doc ann
label typeName = "---- GQL " <> pretty (unpackName typeName) <> " -------------------------------\n"

optional :: ([a] -> Doc n) -> [a] -> Doc n
optional _ [] = ""
optional f xs = " " <> f xs

renderImport :: (Text, [Text]) -> Doc ann
renderImport (src, ls) =
  "import" <+> pretty src
    <> optional (tupled . map pretty) ls
