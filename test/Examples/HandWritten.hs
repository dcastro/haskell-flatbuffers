{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.HandWritten where

import           Control.Exception.Safe        ( throwM )

import           Data.Int
import           Data.Text                     ( Text )
import           Data.Word

import           FlatBuffers.FileIdentifier    ( HasFileIdentifier(..), unsafeFileIdentifier )
import           FlatBuffers.Internal.Positive ( Positive(getPositive) )
import           FlatBuffers.Read
import           FlatBuffers.Write


----------------------------------
---------- Primitives ------------
----------------------------------
data Primitives

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
  writeTable
    [ (optionalDef 1 . inline) word8    a
    , (optionalDef 1 . inline) word16   b
    , (optionalDef 1 . inline) word32   c
    , (optionalDef 1 . inline) word64   d
    , (optionalDef 1 . inline) int8     e
    , (optionalDef 1 . inline) int16    f
    , (optionalDef 1 . inline) int32    g
    , (optionalDef 1 . inline) int64    h
    , (optionalDef 1 . inline) float    i
    , (optionalDef 1 . inline) double   j
    , (optionalDef False . inline) bool k
    ]

getPrimitives'a :: ReadCtx m => Table Primitives -> m Word8
getPrimitives'b :: ReadCtx m => Table Primitives -> m Word16
getPrimitives'c :: ReadCtx m => Table Primitives -> m Word32
getPrimitives'd :: ReadCtx m => Table Primitives -> m Word64
getPrimitives'e :: ReadCtx m => Table Primitives -> m Int8
getPrimitives'f :: ReadCtx m => Table Primitives -> m Int16
getPrimitives'g :: ReadCtx m => Table Primitives -> m Int32
getPrimitives'h :: ReadCtx m => Table Primitives -> m Int64
getPrimitives'i :: ReadCtx m => Table Primitives -> m Float
getPrimitives'j :: ReadCtx m => Table Primitives -> m Double
getPrimitives'k :: ReadCtx m => Table Primitives -> m Bool
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
  = ColorRed
  | ColorGreen
  | ColorBlue
  | ColorGray
  | ColorBlack
  deriving (Eq, Show, Read, Ord, Bounded)

{-# INLINE toColor #-}
toColor :: Word16 -> Maybe Color
toColor n =
  case n of
    0 -> Just ColorRed
    1 -> Just ColorGreen
    2 -> Just ColorBlue
    5 -> Just ColorGray
    8 -> Just ColorBlack
    _ -> Nothing
    
{-# INLINE fromColor #-}
fromColor :: Color -> Word16
fromColor n =
  case n of
    ColorRed   -> 0
    ColorGreen -> 1
    ColorBlue  -> 2
    ColorGray  -> 5
    ColorBlack -> 8

----------------------------------
------------- Enums --------------
----------------------------------
data Enums

enums :: Maybe Word16 -> Maybe (WriteStruct StructWithEnum) -> [Word16] -> Maybe [WriteStruct StructWithEnum] -> WriteTable Enums
enums x1 x2 x3 x4 = writeTable
  [ (optionalDef 2 . inline) word16 x1
  , optional unWriteStruct x2
  , (writeVector . inline) word16 x3
  , (optional . writeVector) unWriteStruct x4
  ]

getEnums'x :: ReadCtx m => Table Enums -> m Word16
getEnums'x = readTableFieldWithDef readWord16 0 2

getEnums'y :: ReadCtx m => Table Enums -> m (Maybe (Struct StructWithEnum))
getEnums'y = readTableFieldOpt readStruct' 1

getEnums'xs :: ReadCtx m => Table Enums -> m (Vector Word16)
getEnums'xs = readTableFieldReq (readPrimVector Word16Vec) 2 "xs"

getEnums'ys :: ReadCtx m => Table Enums -> m (Maybe (Vector (Struct StructWithEnum)))
getEnums'ys = readTableFieldOpt (readStructVector 6) 3



data StructWithEnum

structWithEnum :: Int8 -> Word16 -> Int8 -> WriteStruct StructWithEnum
structWithEnum x1 x2 x3 = writeStruct 2
  [ padded 1 (int8 x1)
  , word16 x2
  , padded 1 (int8 x3)]

getStructWithEnum'x :: ReadCtx m => Struct StructWithEnum -> m Int8
getStructWithEnum'x = readStructField readInt8 0

getStructWithEnum'y :: ReadCtx m => Struct StructWithEnum -> m Word16
getStructWithEnum'y = readStructField readWord16 2

getStructWithEnum'z :: ReadCtx m => Struct StructWithEnum -> m Int8
getStructWithEnum'z = readStructField readInt8 4


----------------------------------
------------- Sword -------------
----------------------------------
data Sword

sword :: Maybe Text -> WriteTable Sword
sword x1 = writeTable [optional text x1]

getSword'x :: ReadCtx m => Table Sword -> m (Maybe Text)
getSword'x = readTableFieldOpt readText 0

----------------------------------
------------- Axe -------------
----------------------------------
data Axe

axe :: Maybe Int32 -> WriteTable Axe
axe x1 = writeTable [(optionalDef 0 . inline) int32 x1]

getAxe'y :: ReadCtx m => Table Axe -> m Int32
getAxe'y = readTableFieldWithDef readInt32 0 0
----------------------------------
------------- Weapon --------------
----------------------------------
data Weapon
  = Weapon'Sword !(Table Sword)
  | Weapon'Axe !(Table Axe)

class EncodeWeapon a where
  weapon :: WriteTable a -> WriteUnion Weapon

instance EncodeWeapon Sword where
  weapon = writeUnion 1

instance EncodeWeapon Axe where
  weapon = writeUnion 2

readWeapon :: ReadCtx m => Positive Word8 -> PositionInfo -> m (Union Weapon)
readWeapon n pos =
  case getPositive n of
    1  -> Union . Weapon'Sword <$> readTable pos
    2  -> Union . Weapon'Axe <$> readTable pos
    n' -> pure $ UnionUnknown n'

----------------------------------
------- TableWithUnion -----------
----------------------------------
data TableWithUnion

tableWithUnion :: Maybe (WriteUnion Weapon) -> WriteUnion Weapon -> WriteTable TableWithUnion
tableWithUnion x1 x2 = writeTable
  [ optional writeUnionType x1
  , optional writeUnionValue x1
  , writeUnionType x2
  , writeUnionValue x2
  ]

getTableWithUnion'uni :: ReadCtx m => Table TableWithUnion -> m (Union Weapon)
getTableWithUnion'uni = readTableFieldUnion readWeapon 0

getTableWithUnion'uniReq :: ReadCtx m => Table TableWithUnion -> m (Union Weapon)
getTableWithUnion'uniReq = readTableFieldUnion readWeapon 2

----------------------------------
------- VectorOfUnions -----------
----------------------------------
data VectorOfUnions

vectorOfUnions :: Maybe [WriteUnion Weapon] -> [WriteUnion Weapon] -> WriteTable VectorOfUnions
vectorOfUnions x1 x2 = writeTable
  [ x1t
  , x1v
  , x2t
  , x2v
  ]
  where
    (x1t, x1v) = writeUnionVectorOpt x1
    (x2t, x2v) = writeUnionVectorReq x2

getVectorOfUnions'xs :: ReadCtx m => Table VectorOfUnions -> m (Maybe (Vector (Union Weapon)))
getVectorOfUnions'xs = readTableFieldUnionVectorOpt readWeapon 0

getVectorOfUnions'xsReq :: ReadCtx m => Table VectorOfUnions -> m (Vector (Union Weapon))
getVectorOfUnions'xsReq = readTableFieldUnionVectorReq readWeapon 2 "xsReq"

----------------------------------
----------- ThreeBytes -----------
----------------------------------
data ThreeBytes

threeBytes :: Word8 -> Word8 -> Word8 -> WriteStruct ThreeBytes
threeBytes a b c =
  writeStruct 1
    [ word8 a
    , word8 b
    , word8 c
    ]

getThreeBytes'a :: ReadCtx m => Struct ThreeBytes -> m Word8
getThreeBytes'a = readStructField readWord8 0

getThreeBytes'b :: ReadCtx m => Struct ThreeBytes -> m Word8
getThreeBytes'b = readStructField readWord8 1

getThreeBytes'c :: ReadCtx m => Struct ThreeBytes -> m Word8
getThreeBytes'c = readStructField readWord8 2

----------------------------------
------- VectorOfStructs ----------
----------------------------------

data VectorOfStructs

vectorOfStructs :: Maybe [WriteStruct ThreeBytes] -> WriteTable VectorOfStructs
vectorOfStructs x1 = writeTable
  [ (optional . writeVector) unWriteStruct x1
  ]

getVectorOfStructs'xs :: ReadCtx m => Table VectorOfStructs -> m (Maybe (Vector (Struct ThreeBytes)))
getVectorOfStructs'xs = readTableFieldOpt (readStructVector 3) 0


----------------------------------
------------- Align --------------
----------------------------------
data Align1

align1 :: Int16 -> WriteStruct Align1
align1 a = writeStruct 4
  [ padded 2 (int16 a)
  ]

getAlign1'x :: ReadCtx m => Struct Align1 -> m Int16
getAlign1'x = readStructField readInt16 0


data Align2

align2 :: Int16 -> Word64 -> Word8 -> WriteStruct Align2
align2 a b c = writeStruct 8
  [ padded 6 (int16 a)
  , word64 b
  , padded 7 (word8 c)
  ]

getAlign2'x :: Struct Align2 -> Struct Align1
getAlign2'x = readStructField readStruct 0

getAlign2'y :: ReadCtx m => Struct Align2 -> m Word64
getAlign2'y = readStructField readWord64 8

getAlign2'z :: ReadCtx m => Struct Align2 -> m Word8
getAlign2'z = readStructField readWord8 16


data AlignT

alignT :: Maybe (WriteStruct Align1) -> Maybe (WriteStruct Align2) -> Maybe [WriteStruct Align1] -> Maybe [WriteStruct Align2] -> WriteTable AlignT
alignT a b c d = writeTable
  [ optional unWriteStruct a
  , optional unWriteStruct b
  , (optional . writeVector) unWriteStruct c
  , (optional . writeVector) unWriteStruct d
  ]

getAlignT'x :: ReadCtx m => Table AlignT -> m (Maybe (Struct Align1))
getAlignT'x = readTableFieldOpt readStruct' 0

getAlignT'y :: ReadCtx m => Table AlignT -> m (Maybe (Struct Align2))
getAlignT'y = readTableFieldOpt readStruct' 1

getAlignT'xs :: ReadCtx m => Table AlignT -> m (Maybe (Vector (Struct Align1)))
getAlignT'xs = readTableFieldOpt (readStructVector 4) 2

getAlignT'ys :: ReadCtx m => Table AlignT -> m (Maybe (Vector (Struct Align2)))
getAlignT'ys = readTableFieldOpt (readStructVector 24) 3

