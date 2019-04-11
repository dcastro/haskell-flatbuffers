{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Examples.HandWritten where

import           Control.Exception.Safe     ( throwM )

import           Data.Int
import           Data.Text                  ( Text )
import           Data.Word

import           FlatBuffers.FileIdentifier ( HasFileIdentifier(..), unsafeFileIdentifier )
import           FlatBuffers.Read
import           FlatBuffers.Write

----------------------------------
---------- Primitives ------------
----------------------------------
newtype Primitives =
  Primitives Table

instance HasFileIdentifier Primitives where
  getFileIdentifier = unsafeFileIdentifier "PRIM"

primitives ::
     Maybe Word8
  -> Maybe Word16
  -> Maybe Word32
  -> Maybe Word64
  -> Maybe Int8
  -> Maybe Int16
  -> Maybe Int32
  -> Maybe Int64
  -> Maybe Float
  -> Maybe Double
  -> Maybe Bool
  -> WriteTable Primitives
primitives a b c d e f g h i j k =
  writeTable [w a, w b, w c, w d, w e, w f, w g, w h, w i, w j, w k]

getPrimitives'a :: ReadCtx m => Primitives -> m Word8
getPrimitives'b :: ReadCtx m => Primitives -> m Word16
getPrimitives'c :: ReadCtx m => Primitives -> m Word32
getPrimitives'd :: ReadCtx m => Primitives -> m Word64
getPrimitives'e :: ReadCtx m => Primitives -> m Int8
getPrimitives'f :: ReadCtx m => Primitives -> m Int16
getPrimitives'g :: ReadCtx m => Primitives -> m Int32
getPrimitives'h :: ReadCtx m => Primitives -> m Int64
getPrimitives'i :: ReadCtx m => Primitives -> m Float
getPrimitives'j :: ReadCtx m => Primitives -> m Double
getPrimitives'k :: ReadCtx m => Primitives -> m Bool
getPrimitives'a = readTableFieldWithDef readWord8   0 1
getPrimitives'b = readTableFieldWithDef readWord16  1 1
getPrimitives'c = readTableFieldWithDef readWord32  2 1
getPrimitives'd = readTableFieldWithDef readWord64  3 1
getPrimitives'e = readTableFieldWithDef readInt8    4 1
getPrimitives'f = readTableFieldWithDef readInt16   5 1
getPrimitives'g = readTableFieldWithDef readInt32   6 1
getPrimitives'h = readTableFieldWithDef readInt64   7 1
getPrimitives'i = readTableFieldWithDef readFloat   8 1
getPrimitives'j = readTableFieldWithDef readDouble  9 1
getPrimitives'k = readTableFieldWithDef readBool    10 False

----------------------------------
------------- Color --------------
----------------------------------
data Color
  = Red
  | Green
  | Blue
  | Gray
  | Black
  deriving (Eq, Show, Enum)

instance AsTableField Color where
  w = inline ws
instance AsStructField Color where
  ws x =
    ws $
    case x of
      Red   -> 0 :: Word8
      Green -> 1 :: Word8
      Blue  -> 2 :: Word8
      Gray  -> 5 :: Word8
      Black -> 8 :: Word8

readColor :: ReadCtx m => Position -> m Color
readColor p =
  readWord8 p >>= \n ->
    case n of
      0 -> pure Red
      1 -> pure Green
      2 -> pure Blue
      5 -> pure Gray
      8 -> pure Black
      _ -> throwM $ EnumUnknown "Color" (fromIntegral @Word8 @Word64 n)

----------------------------------
------------- Enums --------------
----------------------------------
newtype Enums =
  Enums Table

enums :: Maybe Color -> WriteTable Enums
enums x1 = writeTable [w x1]

getEnums'x :: ReadCtx m => Enums -> m Color
getEnums'x = readTableFieldWithDef readColor 0 Blue

----------------------------------
------------- UnionA -------------
----------------------------------
newtype UnionA =
  UnionA Table

unionA :: Maybe Text -> WriteTable UnionA
unionA x1 = writeTable [w x1]

getUnionA'x :: ReadCtx m => ReadMode Text a -> UnionA -> m a
getUnionA'x = readTableField readText 0 "x"

----------------------------------
------------- UnionB -------------
----------------------------------
newtype UnionB =
  UnionB Table

unionB :: Maybe Int32 -> WriteTable UnionB
unionB x1 = writeTable [w x1]

getUnionB'y :: ReadCtx m => UnionB -> m Int32
getUnionB'y = readTableFieldWithDef readInt32 0 0

----------------------------------
------------- Union --------------
----------------------------------
data Union
  = Union'UnionA !UnionA
  | Union'UnionB !UnionB

class EncodeUnion a where
  union :: WriteTable a -> WriteUnion Union

instance EncodeUnion UnionA where
  union = writeUnion 1

instance EncodeUnion UnionB where
  union = writeUnion 2

readUnion :: ReadCtx m => Word8 -> Position -> m (Maybe Union)
readUnion n pos =
  case n of
    0 -> pure Nothing
    1 -> Just . Union'UnionA <$> readTable pos
    2 -> Just . Union'UnionB <$> readTable pos
    _ -> throwM $ UnionUnknown "Union" n

----------------------------------
------- TableWithUnion -----------
----------------------------------
newtype TableWithUnion =
  TableWithUnion Table

tableWithUnion :: Maybe (WriteUnion Union) -> WriteTable TableWithUnion
tableWithUnion x1 =
  writeTable [wType x1, wValue x1]


getTableWithUnion'uni :: ReadCtx m => TableWithUnion -> m (Maybe Union)
getTableWithUnion'uni = readTableFieldUnion readUnion 0

----------------------------------
------- VectorOfUnions -----------
----------------------------------
newtype VectorOfUnions =
  VectorOfUnions Table

vectorOfUnions :: Maybe [WriteUnion Union] -> WriteTable VectorOfUnions
vectorOfUnions x1 =
  writeTable [wType x1, wValue x1]

getVectorOfUnions'xs :: ReadCtx m => ReadMode (Vector (Maybe Union)) a -> VectorOfUnions -> m a
getVectorOfUnions'xs = readTableFieldUnionVector readUnion 0 "xs"

----------------------------------
----------- ThreeBytes -----------
----------------------------------
newtype ThreeBytes = ThreeBytes Struct

threeBytes :: Word8 -> Word8 -> Word8 -> WriteStruct ThreeBytes
threeBytes a b c =
  writeStruct 1
    [ ws a
    , ws b
    , ws c
    ]

getThreeBytes'a :: ReadCtx m => ThreeBytes -> m Word8
getThreeBytes'a = readStructField readWord8 0

getThreeBytes'b :: ReadCtx m => ThreeBytes -> m Word8
getThreeBytes'b = readStructField readWord8 1

getThreeBytes'c :: ReadCtx m => ThreeBytes -> m Word8
getThreeBytes'c = readStructField readWord8 2

----------------------------------
------- VectorOfStructs ----------
----------------------------------

newtype VectorOfStructs = VectorOfStructs Table

vectorOfStructs :: Maybe [WriteStruct ThreeBytes] -> WriteTable VectorOfStructs
vectorOfStructs x1 = writeTable [w x1]

getVectorOfStructs'xs :: ReadCtx m => ReadMode (Vector ThreeBytes) a -> VectorOfStructs -> m a
getVectorOfStructs'xs = readTableField (readVector (pure . readStruct) 3) 0 "xs"
