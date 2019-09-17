{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

module FlatBuffers.Internal.Types where

import Data.Word

-- | Metadata for a struct type.
class IsStruct a where
  structAlignmentOf :: Alignment
  structSizeOf      :: InlineSize

-- | The number of bytes occupied by a piece of data that's stored "inline"
--
-- "inline" here means "stored directly in a table or a vector, and not by reference".
-- E.g.: numeric types, booleans, structs, offsets.
newtype InlineSize = InlineSize { unInlineSize :: Word16 }
  deriving newtype (Show, Eq, Num, Enum, Ord, Real, Integral, Bounded)

-- | The memory alignment (in bytes) for a piece of data in a flatbuffer.
-- E.g., `Data.Int.Int32` are always aligned to 4 bytes.
-- This number should always be a power of 2 in the range [1, 16].
newtype Alignment = Alignment { unAlignment :: Word8 }
  deriving newtype (Show, Eq, Num, Enum, Ord, Real, Integral, Bounded)

