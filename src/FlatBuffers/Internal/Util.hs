module FlatBuffers.Internal.Util where

import           Data.Bits (Bits, (.&.))
import           Data.Text (Text)

isPowerOfTwo :: (Num a, Bits a) => a -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo n = (n .&. (n - 1)) == 0

roundUpToNearestMultipleOf :: Integral n => n -> n -> n
roundUpToNearestMultipleOf x y =
  case x `rem` y of
    0 -> x
    remainder -> (y - remainder) + x

-- | Maps a value of type @a@ into a string that can be displayed to the user.
class Display a where
  display :: a -> Text

instance Display Text where
  display = id