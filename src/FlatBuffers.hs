module FlatBuffers
  (
    -- * TemplateHaskell
    TH.mkFlatBuffers
  , TH.defaultOptions
  , TH.Options(..)

    -- * Creating a flatbuffer
  , W.encode
  , W.encodeWithFileIdentifier
  , W.none

    -- * Reading a flatbuffer
  , R.decode
  , R.checkFileIdentifier

  -- * File Identifier
  , FI.FileIdentifier
  , FI.HasFileIdentifier(..)

  -- * Types
  , W.WriteStruct
  , W.WriteTable
  , W.WriteUnion
  , R.Struct
  , R.Table
  , R.Union(..)
  , T.InlineSize(..)
  , T.Alignment(..)
  , T.IsStruct(..)
  , R.ReadError
  ) where

import           FlatBuffers.Internal.Compiler.TH    as TH
import           FlatBuffers.Internal.FileIdentifier as FI
import           FlatBuffers.Internal.Read           as R
import           FlatBuffers.Internal.Types          as T
import           FlatBuffers.Internal.Write          as W

