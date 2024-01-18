module FlatBuffers.Internal.Build where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.Int
import Data.Word

{-# INLINE buildWord8 #-}
buildWord8 :: Word8 -> Builder
buildWord8 = B.word8

{-# INLINE buildWord16 #-}
buildWord16 :: Word16 -> Builder
buildWord16 = B.word16LE

{-# INLINE buildWord32 #-}
buildWord32 :: Word32 -> Builder
buildWord32 = B.word32LE

{-# INLINE buildWord64 #-}
buildWord64 :: Word64 -> Builder
buildWord64 = B.word64LE

{-# INLINE buildInt8 #-}
buildInt8 :: Int8 -> Builder
buildInt8 = B.int8

{-# INLINE buildInt16 #-}
buildInt16 :: Int16 -> Builder
buildInt16 = B.int16LE

{-# INLINE buildInt32 #-}
buildInt32 :: Int32 -> Builder
buildInt32 = B.int32LE

{-# INLINE buildInt64 #-}
buildInt64 :: Int64 -> Builder
buildInt64 = B.int64LE

{-# INLINE buildFloat #-}
buildFloat :: Float -> Builder
buildFloat = B.floatLE

{-# INLINE buildDouble #-}
buildDouble :: Double -> Builder
buildDouble = B.doubleLE

{-# INLINE buildBool #-}
buildBool :: Bool -> Builder
buildBool = buildWord8 . boolToWord8

{-# INLINE buildPadding #-}
buildPadding :: Int32 -> Builder
buildPadding !n =
  foldMap (\_ -> B.word8 0) [0..n-1]

{-# INLINE boolToWord8 #-}
boolToWord8 :: Bool -> Word8
boolToWord8 False = 0
boolToWord8 True  = 1
