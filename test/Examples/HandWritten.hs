{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.HandWritten where

import           Control.Exception.Safe        ( throwM )

import           Data.Int
import           Data.Text                     ( Text )
import           Data.Word

import           FlatBuffers.FileIdentifier    ( HasFileIdentifier(..), unsafeFileIdentifier )
import           FlatBuffers.Internal.Positive ( Positive(getPositive) )
import qualified FlatBuffers.Internal.Write    as W
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
  deriving (Eq, Show, Enum, Ord)

instance AsTableField Color where
  w = inline ws
instance AsStructField Color where
  ws x =
    ws $
    case x of
      Red   -> 0 :: Word16
      Green -> 1 :: Word16
      Blue  -> 2 :: Word16
      Gray  -> 5 :: Word16
      Black -> 8 :: Word16

readColor :: (ReadCtx m, HasPosition a) => a -> m Color
readColor p =
  readWord16 p >>= \n ->
    case n of
      0 -> pure Red
      1 -> pure Green
      2 -> pure Blue
      5 -> pure Gray
      8 -> pure Black
      _ -> throwM $ EnumUnknown "Color" (toInteger n)

----------------------------------
------------- Enums --------------
----------------------------------
newtype Enums =
  Enums Table

enums :: Maybe Color -> Maybe (WriteStruct StructWithEnum) -> [Color] -> Maybe [WriteStruct StructWithEnum] -> WriteTable Enums
enums x1 x2 x3 x4 = writeTable [w x1, w x2, w x3, w x4]

getEnums'x :: ReadCtx m => Enums -> m Color
getEnums'x = readTableFieldWithDef readColor 0 Blue

getEnums'y :: ReadCtx m => ReadMode StructWithEnum a -> Enums -> m a
getEnums'y = readTableField readStruct' 1 "y"

getEnums'xs :: ReadCtx m => Enums -> m (Vector Color)
getEnums'xs = readTableField (readVector readColor 2) 2 "xs" req

getEnums'ys :: ReadCtx m => ReadMode (Vector StructWithEnum) a -> Enums -> m a
getEnums'ys = readTableField (readVector readStruct' 6) 3 "ys"



newtype StructWithEnum = StructWithEnum Struct

structWithEnum :: Int8 -> Color -> Int8 -> WriteStruct StructWithEnum
structWithEnum x1 x2 x3 = writeStruct 2 [W.padded 1 $ ws x1, ws x2, W.padded 1 $ ws x3]

getStructWithEnum'x :: ReadCtx m => StructWithEnum -> m Int8
getStructWithEnum'x = readStructField readInt8 0

getStructWithEnum'y :: ReadCtx m => StructWithEnum -> m Color
getStructWithEnum'y = readStructField readColor 2

getStructWithEnum'z :: ReadCtx m => StructWithEnum -> m Int8
getStructWithEnum'z = readStructField readInt8 4


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

readUnion :: ReadCtx m => Positive Word8 -> PositionInfo -> m Union
readUnion n pos =
  case getPositive n of
    1 -> Union'UnionA <$> readTable pos
    2 -> Union'UnionB <$> readTable pos
    _ -> throwM $ UnionUnknown "Union" (getPositive n)

----------------------------------
------- TableWithUnion -----------
----------------------------------
newtype TableWithUnion =
  TableWithUnion Table

tableWithUnion :: Maybe (WriteUnion Union) -> WriteTable TableWithUnion
tableWithUnion x1 =
  writeTable [wType x1, wValue x1]

getTableWithUnion'uni :: ReadCtx m => ReadMode Union a -> TableWithUnion -> m a
getTableWithUnion'uni = readTableFieldUnion readUnion 0 "uni"

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
getVectorOfStructs'xs = readTableField (readVector readStruct' 3) 0 "xs"


----------------------------------
------------- Align --------------
----------------------------------
newtype Align1 = Align1 Struct

align1 :: Int16 -> WriteStruct Align1
align1 a = writeStruct 4 [ W.padded 2 $ ws a ]

getAlign1'x :: ReadCtx m => Align1 -> m Int16
getAlign1'x = readStructField readInt16 0


newtype Align2 = Align2 Struct

align2 :: Int16 -> Word64 -> Word8 -> WriteStruct Align2
align2 a b c = writeStruct 8 [ W.padded 6 $ ws a, ws b, W.padded 7 $ ws c ]

getAlign2'x :: Align2 -> Align1
getAlign2'x = readStructField readStruct 0

getAlign2'y :: ReadCtx m => Align2 -> m Word64
getAlign2'y = readStructField readWord64 8

getAlign2'z :: ReadCtx m => Align2 -> m Word8
getAlign2'z = readStructField readWord8 16


newtype AlignT = AlignT Table

alignT :: Maybe (WriteStruct Align1) -> Maybe (WriteStruct Align2) -> Maybe [WriteStruct Align1] -> Maybe [WriteStruct Align2] -> WriteTable AlignT
alignT a b c d = writeTable [ w a, w b, w c, w d ]

getAlignT'x :: ReadCtx m => ReadMode Align1 a ->  AlignT -> m a
getAlignT'x = readTableField readStruct' 0 "x"

getAlignT'y :: ReadCtx m => ReadMode Align2 a ->  AlignT -> m a
getAlignT'y = readTableField readStruct' 1 "y"

getAlignT'xs :: ReadCtx m => ReadMode (Vector Align1) a -> AlignT -> m a
getAlignT'xs = readTableField (readVector readStruct' 4) 2 "xs"

getAlignT'ys :: ReadCtx m => ReadMode (Vector Align2) a -> AlignT -> m a
getAlignT'ys = readTableField (readVector readStruct' 24) 3 "ys"

