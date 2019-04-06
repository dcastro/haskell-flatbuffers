{-# LANGUAGE GADTs #-}

module FlatBuffers.Write
  ( WriteUnion(..)
  , WriteTable(..)
  , WriteStruct(..)
  , encode
  , none
  , AsUnion(..)
  , AsTableField(..)
  , AsStructField(..)
  , writeTable
  , writeUnion
  , writeStruct
  , F.padded
  , F.inline
  ) where

import           Data.Bifunctor             (bimap)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Int
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Text                  (Text)
import           Data.Word
import           FlatBuffers.Constants      (InlineSize)
import           FlatBuffers.Internal.Write
import qualified FlatBuffers.Internal.Write as F

encode :: WriteTable a -> BSL.ByteString
encode (WriteTable table) = root table

newtype WriteTable a = WriteTable Field

newtype WriteStruct a = WriteStruct Field

data WriteUnion a
  = None
  | Some !(Word8, Field)

writeTable :: [Field] -> WriteTable a
writeTable = WriteTable . F.table

writeStruct :: InlineSize -> NonEmpty InlineField -> WriteStruct a
writeStruct structAlign xs = WriteStruct (F.struct structAlign xs)

writeUnion :: Word8 -> WriteTable a -> WriteUnion b
writeUnion n (WriteTable t) = Some (n, t)

none :: WriteUnion a
none = None

-- | Writes a 'union-like' value to a table.
-- | Unions are a special-case in that they generate two table fields, instead of just one.
class AsUnion a where
  wType :: a -> Field
  wValue :: a -> Field

instance AsUnion (WriteUnion a) where
  wType (Some (t, _)) = inline word8 t
  wType None          = inline word8 0
  wValue (Some (_, v)) = v
  wValue None          = missing

instance AsUnion a => AsUnion (Maybe a) where
  wType (Just a) = wType a
  wType Nothing  = missing
  wValue (Just a) = wValue a
  wValue Nothing  = missing


-- | Writes a vector of unions to a table.
instance a ~ WriteUnion b => AsUnion [a] where
  wType = vector . fmap wType
  wValue = vector . fmap f
    where
      -- in a vector of unions, a `none` value is encoded as the circular reference 0.
      f None = inline int32 0
      f x    = wValue x



-- | Writes a value to a table.
class AsTableField a where
  w :: a -> Field

instance AsTableField Field where
  w = id

instance AsTableField (WriteTable a) where
  w (WriteTable t) = t

instance AsTableField (WriteStruct a) where
  w (WriteStruct t) = t

instance AsTableField a => AsTableField (Maybe a) where
  w (Just x) = w x
  w Nothing = missing

instance AsTableField a => AsTableField [a] where
  w = vector . fmap w
  
instance AsTableField Text where
  w = text

instance AsTableField Word8 where w = inline ws
instance AsTableField Word16 where w = inline ws
instance AsTableField Word32 where w = inline ws
instance AsTableField Word64 where w = inline ws
instance AsTableField Int8 where w = inline ws
instance AsTableField Int16 where w = inline ws
instance AsTableField Int32 where w = inline ws
instance AsTableField Int64 where w = inline ws
instance AsTableField Float where w = inline ws
instance AsTableField Double where w = inline ws
instance AsTableField Bool where w = inline ws

-- | Writes a value to a struct.
class AsStructField a where
  ws :: a -> InlineField

instance AsStructField Word8 where ws = word8
instance AsStructField Word16 where ws = word16
instance AsStructField Word32 where ws = word32
instance AsStructField Word64 where ws = word64
instance AsStructField Int8 where ws = int8
instance AsStructField Int16 where ws = int16
instance AsStructField Int32 where ws = int32
instance AsStructField Int64 where ws = int64
instance AsStructField Float where ws = float
instance AsStructField Double where ws = double
instance AsStructField Bool where ws = bool
