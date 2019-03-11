module FlatBuffers.Internal.Util where

import           Data.Bits (Bits, (.&.))

isPowerOfTwo :: (Num a, Bits a) => a -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo n = (n .&. (n - 1)) == 0

roundUpToNearestMultipleOf :: Integral n => n -> n -> n
roundUpToNearestMultipleOf x y =
  case x `rem` y of
    0 -> x
    remainder -> (y - remainder) + x
