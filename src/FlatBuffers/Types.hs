{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FlatBuffers.Types where

import Data.Word

newtype InlineSize = InlineSize { unInlineSize :: Word16 }
  deriving newtype (Show, Eq, Num, Enum, Ord, Real, Integral, Bounded)

newtype Alignment = Alignment { unAlignment :: Word8 }
  deriving newtype (Show, Eq, Num, Enum, Ord, Real, Integral, Bounded)

