{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.Internal.Util where

import           Data.Bits          (Bits, (.&.))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Scientific    (Scientific)

isPowerOfTwo :: (Num a, Bits a) => a -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo n = (n .&. (n - 1)) == 0

roundUpToNearestMultipleOf :: Integral n => n -> n -> n
roundUpToNearestMultipleOf x y =
  case x `rem` y of
    0         -> x
    remainder -> (y - remainder) + x

-- | Maps a value of type @a@ into a string that can be displayed to the user.
class Display a where
  display :: a -> Text

instance Display Text where
  display = id

instance Display a => Display (NonEmpty a) where
  display xs = "[" <> T.intercalate ", " (NE.toList (fmap displayOne xs)) <> "]"
    where
      displayOne x = "'" <> display x <> "'"

instance Display Scientific where
  display = T.pack . show
