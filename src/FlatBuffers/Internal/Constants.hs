{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FlatBuffers.Internal.Constants where

voffsetSize, uoffsetSize, soffsetSize :: Num a => a
voffsetSize = word16Size
uoffsetSize = word32Size
soffsetSize = int32Size
{-# INLINE voffsetSize #-}
{-# INLINE uoffsetSize #-}
{-# INLINE soffsetSize #-}

fileIdentifierSize :: Num a => a
fileIdentifierSize = 4
{-# INLINE fileIdentifierSize #-}

textRefSize, tableRefSize :: Num a => a
textRefSize = uoffsetSize
tableRefSize = uoffsetSize
{-# INLINE textRefSize #-}
{-# INLINE tableRefSize #-}

word8Size, word16Size, word32Size, word64Size :: Num a => a
word8Size = 1
word16Size = 2
word32Size = 4
word64Size = 8
{-# INLINE word8Size #-}
{-# INLINE word16Size #-}
{-# INLINE word32Size #-}
{-# INLINE word64Size #-}

int8Size, int16Size, int32Size, int64Size :: Num a => a
int8Size = 1
int16Size = 2
int32Size = 4
int64Size = 8
{-# INLINE int8Size #-}
{-# INLINE int16Size #-}
{-# INLINE int32Size #-}
{-# INLINE int64Size #-}

boolSize, floatSize, doubleSize :: Num a => a
boolSize = 1
floatSize = 4
doubleSize = 8
{-# INLINE boolSize #-}
{-# INLINE floatSize #-}
{-# INLINE doubleSize #-}


