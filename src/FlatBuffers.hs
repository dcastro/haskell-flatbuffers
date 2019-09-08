module FlatBuffers
  (
    -- * TemplateHaskell
    TH.mkFlatBuffers
  , TH.defaultOptions

    -- * Creating a flatbuffer
  , W.encode
  , W.encodeWithFileIdentifier
  , W.WriteStruct
  , W.WriteTable
  , W.WriteUnion
  , W.none

    -- * Reading a flatbuffer
  , R.decode
  , R.checkFileIdentifier
  , R.Struct
  , R.Table
  , R.Union(..)
  , R.ReadError(..)

  -- * File Identifier
  , FI.FileIdentifier
  , FI.HasFileIdentifier(..)
  ) where

import           FlatBuffers.Internal.Compiler.TH    as TH
import           FlatBuffers.Internal.FileIdentifier as FI
import           FlatBuffers.Internal.Read           as R
import           FlatBuffers.Internal.Write          as W


