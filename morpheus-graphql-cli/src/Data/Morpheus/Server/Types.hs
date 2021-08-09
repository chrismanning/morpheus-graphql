module Data.Morpheus.Server.Types where

import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig,
  )
import Language.Haskell.TH (Q)
import Relude (ReaderT)

type ServerDec = ReaderT CodeGenConfig Q
