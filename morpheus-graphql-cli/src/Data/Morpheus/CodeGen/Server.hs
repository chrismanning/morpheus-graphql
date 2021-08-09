module Data.Morpheus.CodeGen.Server (render) where

import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Server.CodeGen.Transform
  ( parseServerTypeDefinitions,
  )
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
  )
import Data.Morpheus.CodeGen.Server.Rendering.Render
  ( renderDocument,
  )

render :: String -> ByteString -> Either ByteString ByteString
render moduleName doc = case parseServerTypeDefinitions
  (CodeGenConfig {namespace = False})
  doc of
  Left errors -> Left $ LB.pack errors
  Right lib -> Right $ renderDocument moduleName lib
