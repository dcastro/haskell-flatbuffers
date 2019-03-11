{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FlatBuffers.Constants where

import Data.Word (Word16)

newtype InlineSize = InlineSize { unInlineSize :: Word16 }
  deriving (Show, Eq, Num, Enum, Ord, Real, Integral, Bounded)

voffsetSize, uoffsetSize, soffsetSize :: InlineSize
voffsetSize = word16Size
uoffsetSize = word32Size
soffsetSize = int32Size

textSize, tableSize :: InlineSize
textSize = uoffsetSize
tableSize = uoffsetSize

word8Size, word16Size, word32Size, word64Size :: InlineSize
(word8Size, word16Size, word32Size, word64Size) = (1, 2, 4, 8)

int8Size, int16Size, int32Size, int64Size :: InlineSize
(int8Size, int16Size, int32Size, int64Size) = (1, 2, 4, 8)

boolSize, floatSize, doubleSize :: InlineSize
(boolSize, floatSize, doubleSize) = (1, 4, 8)


