{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare.Type
  ( declareType,
  )
where

import Data.Morpheus.CodeGen.Server.Internal.AST
  ( DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    ServerConstructorDefinition (..),
    ServerFieldDefinition (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Internal.TH
  ( apply,
    declareTypeRef,
    toCon,
    toName,
  )
import Data.Morpheus.Server.TH.Utils
  ( m',
    m_,
  )
import Data.Morpheus.Types
  ( SubscriptionField,
    TypeGuard,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
    TypeName,
  )
import Language.Haskell.TH
import Relude hiding (Type)

declareType :: ServerTypeDefinition -> [Dec]
declareType (ServerInterfaceDefinition name interfaceName unionName) =
  [ TySynD
      (toName name)
      [PlainTV m_]
      (apply ''TypeGuard [apply interfaceName [m'], apply unionName [m']])
  ]
declareType ServerTypeDefinition {tKind = KindScalar} = []
declareType
  ServerTypeDefinition
    { tName,
      tCons,
      derives,
      typeParameters
    } = [DataD [] (toName tName) vars Nothing cons [derivings]]
    where
      derivings = DerivClause Nothing (map (ConT . genName) derives)
      cons = map declareCons tCons
      vars = map (PlainTV . toName) typeParameters

genName :: DerivingClass -> Name
genName GENERIC = ''Generic
genName SHOW = ''Show

declareCons :: ServerConstructorDefinition -> Con
declareCons ServerConstructorDefinition {constructorName, constructorFields} =
  RecC
    (toName constructorName)
    (map declareField constructorFields)

declareField :: ServerFieldDefinition -> (Name, Bang, Type)
declareField
  ServerFieldDefinition
    { fieldName,
      isParametrized,
      fieldType,
      wrappers
    } =
    ( toName fieldName,
      Bang NoSourceUnpackedness NoSourceStrictness,
      foldr applyWrapper (declareTypeRef renderTypeName fieldType) wrappers
    )
    where
      renderTypeName :: TypeName -> Type
      renderTypeName
        | isParametrized = (`apply` [m'])
        | otherwise = toCon

applyWrapper :: FIELD_TYPE_WRAPPER -> Type -> Type
applyWrapper MONAD = AppT m'
applyWrapper SUBSCRIPTION = AppT (ConT ''SubscriptionField)
applyWrapper (ARG typeName) = InfixT (ConT (toName typeName)) ''Function

type Function = (->)
