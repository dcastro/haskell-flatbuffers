{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module FlatBuffers.Types where

import Data.Word

class IsStruct a where
  structAlignmentOf :: Alignment
  structSizeOf      :: InlineSize

newtype InlineSize = InlineSize { unInlineSize :: Word16 }
  deriving newtype (Show, Eq, Num, Enum, Ord, Real, Integral, Bounded)

-- | The memory alignment for a piece of data in a flatbuffer.
-- E.g., int32 are always aligned to 4 bytes.
-- This number should always be in the range [1, 16].
newtype Alignment = Alignment { unAlignment :: Word8 }
  deriving newtype (Show, Eq, Num, Enum, Ord, Real, Integral, Bounded)

