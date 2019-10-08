{-# LANGUAGE FlexibleInstances #-}

module FlatBuffers.Internal.Compiler.Display where

import           Data.Int
import qualified Data.List          as List
import           Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T
import           Data.Word

-- | Maps a value of type @a@ into a string that can be displayed to the user.
class Display a where
  display :: a -> String

instance {-# OVERLAPPING #-} Display String where
  display = id

instance Display T.Text where
  display = T.unpack

instance Display a => Display (NonEmpty a) where
  display = display . NE.toList

instance Display a => Display [a] where
  display xs = List.intercalate ", " (fmap displayOne xs)
    where
      displayOne x = "'" <> display x <> "'"

instance Display Int     where display = show
instance Display Integer where display = show
instance Display Int8    where display = show
instance Display Int16   where display = show
instance Display Int32   where display = show
instance Display Int64   where display = show
instance Display Word8   where display = show
instance Display Word16  where display = show
instance Display Word32  where display = show
instance Display Word64  where display = show

