{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.HandWritten where

import           Data.Int
import           Data.Text                     ( Text )
import           Data.Word

import           FlatBuffers.FileIdentifier    ( HasFileIdentifier(..), unsafeFileIdentifier )
import           FlatBuffers.Internal.Positive ( Positive(getPositive) )
import           FlatBuffers.Read
import           FlatBuffers.Write

----------------------------------
---------- Empty table -----------
----------------------------------
data EmptyTable

emptyTable :: WriteTable EmptyTable
emptyTable = writeTable []

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
  -> Maybe Text
  -> WriteTable Primitives
primitives a b c d e f g h i j k l =
  writeTable
    [ (optionalDef 0 . inline) word8    a
    , (optionalDef 0 . inline) word16   b
    , (optionalDef 0 . inline) word32   c
    , (optionalDef 0 . inline) word64   d
    , (optionalDef 0 . inline) int8     e
    , (optionalDef 0 . inline) int16    f
    , (optionalDef 0 . inline) int32    g
    , (optionalDef 0 . inline) int64    h
    , (optionalDef 0 . inline) float    i
    , (optionalDef 0 . inline) double   j
    , (optionalDef False . inline) bool k
    , optional text                     l
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
getPrimitives'l :: ReadCtx m => Table Primitives -> m (Maybe Text)
getPrimitives'a = readTableFieldWithDef readWord8   0 0
getPrimitives'b = readTableFieldWithDef readWord16  1 0
getPrimitives'c = readTableFieldWithDef readWord32  2 0
getPrimitives'd = readTableFieldWithDef readWord64  3 0
getPrimitives'e = readTableFieldWithDef readInt8    4 0
getPrimitives'f = readTableFieldWithDef readInt16   5 0
getPrimitives'g = readTableFieldWithDef readInt32   6 0
getPrimitives'h = readTableFieldWithDef readInt64   7 0
getPrimitives'i = readTableFieldWithDef readFloat   8 0
getPrimitives'j = readTableFieldWithDef readDouble  9 0
getPrimitives'k = readTableFieldWithDef readBool    10 False
getPrimitives'l = readTableFieldOpt     readText    11

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
toColor :: Int16 -> Maybe Color
toColor n =
  case n of
    -2 -> Just ColorRed
    0 -> Just ColorGreen
    1 -> Just ColorBlue
    5 -> Just ColorGray
    8 -> Just ColorBlack
    _ -> Nothing

{-# INLINE fromColor #-}
fromColor :: Color -> Int16
fromColor n =
  case n of
    ColorRed   -> -2
    ColorGreen -> 0
    ColorBlue  -> 1
    ColorGray  -> 5
    ColorBlack -> 8

----------------------------------
------------- Enums --------------
----------------------------------
data Enums

enums :: Maybe Int16 -> Maybe (WriteStruct StructWithEnum) -> Maybe [Int16] -> Maybe [WriteStruct StructWithEnum] -> WriteTable Enums
enums x1 x2 x3 x4 = writeTable
  [ (optionalDef 0 . inline) int16 x1
  , (optional . inline) unWriteStruct x2
  , (optional . writeVector . inline) int16 x3
  , (optional . writeVector . inline) unWriteStruct x4
  ]

getEnums'x :: ReadCtx m => Table Enums -> m Int16
getEnums'x = readTableFieldWithDef readInt16 0 0

getEnums'y :: ReadCtx m => Table Enums -> m (Maybe (Struct StructWithEnum))
getEnums'y = readTableFieldOpt readStruct' 1

getEnums'xs :: ReadCtx m => Table Enums -> m (Maybe (Vector Int16))
getEnums'xs = readTableFieldOpt (readPrimVector Int16Vec) 2

getEnums'ys :: ReadCtx m => Table Enums -> m (Maybe (Vector (Struct StructWithEnum)))
getEnums'ys = readTableFieldOpt (readStructVector 6) 3



data StructWithEnum

structWithEnum :: Int8 -> Int16 -> Int8 -> WriteStruct StructWithEnum
structWithEnum x1 x2 x3 = writeStruct 2
  [ padded 1 (int8 x3)
  , int16 x2
  , padded 1 (int8 x1)
  ]

getStructWithEnum'x :: ReadCtx m => Struct StructWithEnum -> m Int8
getStructWithEnum'x = readStructField readInt8 0

getStructWithEnum'y :: ReadCtx m => Struct StructWithEnum -> m Int16
getStructWithEnum'y = readStructField readInt16 2

getStructWithEnum'z :: ReadCtx m => Struct StructWithEnum -> m Int8
getStructWithEnum'z = readStructField readInt8 4

----------------------------------
------------- Structs ------------
----------------------------------
data Struct1

struct1 :: Word8 -> Int8 -> Int8 -> WriteStruct Struct1
struct1 a b c =
  writeStruct 1
    [ int8 c
    , int8 b
    , word8 a
    ]

getStruct1'x :: ReadCtx m => Struct Struct1 -> m Word8
getStruct1'x = readStructField readWord8 0

getStruct1'y :: ReadCtx m => Struct Struct1 -> m Int8
getStruct1'y = readStructField readInt8 1

getStruct1'z :: ReadCtx m => Struct Struct1 -> m Int8
getStruct1'z = readStructField readInt8 2


data Struct2

struct2 :: Int16 -> WriteStruct Struct2
struct2 a = writeStruct 4
  [ padded 2 (int16 a)
  ]

getStruct2'x :: ReadCtx m => Struct Struct2 -> m Int16
getStruct2'x = readStructField readInt16 0


data Struct3

struct3 :: WriteStruct Struct2 -> Word64 -> Word8 -> WriteStruct Struct3
struct3 a b c = writeStruct 8
  [ padded 7 (word8 c)
  , word64 b
  , padded 4 (unWriteStruct a)
  ]

getStruct3'x :: Struct Struct3 -> Struct Struct2
getStruct3'x = readStructField readStruct 0

getStruct3'y :: ReadCtx m => Struct Struct3 -> m Word64
getStruct3'y = readStructField readWord64 8

getStruct3'z :: ReadCtx m => Struct Struct3 -> m Word8
getStruct3'z = readStructField readWord8 16


data Struct4

struct4 :: WriteStruct Struct2 -> Int8 -> Int64 -> Bool -> WriteStruct Struct4
struct4 a b c d = writeStruct 8
  [ padded 7 (bool d)
  , int64 c
  , padded 3 (int8 b)
  , unWriteStruct a
  ]

getStruct4'w :: Struct Struct4 -> Struct Struct2
getStruct4'w = readStructField readStruct 0

getStruct4'x :: ReadCtx m => Struct Struct4 -> m Int8
getStruct4'x = readStructField readInt8 4

getStruct4'y :: ReadCtx m => Struct Struct4 -> m Int64
getStruct4'y = readStructField readInt64 8

getStruct4'z :: ReadCtx m => Struct Struct4 -> m Bool
getStruct4'z = readStructField readBool 16


data Structs

structs ::
     Maybe (WriteStruct Struct1)
  -> Maybe (WriteStruct Struct2)
  -> Maybe (WriteStruct Struct3)
  -> Maybe (WriteStruct Struct4)
  -> WriteTable Structs
structs x1 x2 x3 x4 = writeTable
  [ (optional . inline) unWriteStruct x1
  , (optional . inline) unWriteStruct x2
  , (optional . inline) unWriteStruct x3
  , (optional . inline) unWriteStruct x4
  ]

getStructs'a :: ReadCtx m => Table Structs -> m (Maybe (Struct Struct1))
getStructs'a = readTableFieldOpt readStruct' 0

getStructs'b :: ReadCtx m => Table Structs -> m (Maybe (Struct Struct2))
getStructs'b = readTableFieldOpt readStruct' 1

getStructs'c :: ReadCtx m => Table Structs -> m (Maybe (Struct Struct3))
getStructs'c = readTableFieldOpt readStruct' 2

getStructs'd :: ReadCtx m => Table Structs -> m (Maybe (Struct Struct4))
getStructs'd = readTableFieldOpt readStruct' 3


----------------------------------
--------- Nested Tables ----------
----------------------------------
data NestedTables

nestedTables :: Maybe (WriteTable Table1) -> WriteTable NestedTables
nestedTables x0 = writeTable
  [ optional unWriteTable x0
  ]

getNestedTables'x :: ReadCtx m => Table NestedTables -> m (Maybe (Table Table1))
getNestedTables'x = readTableFieldOpt readTable 0


data Table1

table1 :: Maybe (WriteTable Table2) -> Maybe Int32 -> WriteTable Table1
table1 x0 x1 = writeTable
  [ optional unWriteTable x0
  , (optionalDef 0 . inline) int32 x1
  ]

getTable1'x :: ReadCtx m => Table Table1 -> m (Maybe (Table Table2))
getTable1'x = readTableFieldOpt readTable 0

getTable1'y :: ReadCtx m => Table Table1 -> m Int16
getTable1'y = readTableFieldWithDef readInt16 1 0


data Table2

table2 :: Maybe Int16 -> WriteTable Table2
table2 x0 = writeTable [ (optionalDef 0 . inline) int16 x0 ]

getTable2'x :: ReadCtx m => Table Table2 -> m Int16
getTable2'x = readTableFieldWithDef readInt16 0 0

----------------------------------
------------- Sword --------------
----------------------------------
data Sword

sword :: Maybe Text -> WriteTable Sword
sword x1 = writeTable [optional text x1]

getSword'x :: ReadCtx m => Table Sword -> m (Maybe Text)
getSword'x = readTableFieldOpt readText 0

----------------------------------
------------- Axe ----------------
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

class WriteWeapon a where
  weapon :: WriteTable a -> WriteUnion Weapon

instance WriteWeapon Sword where
  weapon = writeUnion 1

instance WriteWeapon Axe where
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

tableWithUnion :: WriteUnion Weapon -> WriteTable TableWithUnion
tableWithUnion x1 = writeTable
  [ writeUnionType x1
  , writeUnionValue x1
  ]

getTableWithUnion'uni :: ReadCtx m => Table TableWithUnion -> m (Union Weapon)
getTableWithUnion'uni = readTableFieldUnion readWeapon 1

----------------------------------
------------ Vectors -------------
----------------------------------
data Vectors

vectors ::
     Maybe [Word8]
  -> Maybe [Word16]
  -> Maybe [Word32]
  -> Maybe [Word64]
  -> Maybe [Int8]
  -> Maybe [Int16]
  -> Maybe [Int32]
  -> Maybe [Int64]
  -> Maybe [Float]
  -> Maybe [Double]
  -> Maybe [Bool]
  -> Maybe [Text]
  -> WriteTable Vectors
vectors a b c d e f g h i j k l =
  writeTable
    [ (optional . writeVector . inline) word8    a
    , (optional . writeVector . inline) word16   b
    , (optional . writeVector . inline) word32   c
    , (optional . writeVector . inline) word64   d
    , (optional . writeVector . inline) int8     e
    , (optional . writeVector . inline) int16    f
    , (optional . writeVector . inline) int32    g
    , (optional . writeVector . inline) int64    h
    , (optional . writeVector . inline) float    i
    , (optional . writeVector . inline) double   j
    , (optional . writeVector . inline) bool     k
    , (optional . writeVector)          text     l
    ]

getVectors'a :: ReadCtx m => Table Vectors -> m (Maybe (Vector Word8))
getVectors'b :: ReadCtx m => Table Vectors -> m (Maybe (Vector Word16))
getVectors'c :: ReadCtx m => Table Vectors -> m (Maybe (Vector Word32))
getVectors'd :: ReadCtx m => Table Vectors -> m (Maybe (Vector Word64))
getVectors'e :: ReadCtx m => Table Vectors -> m (Maybe (Vector Int8))
getVectors'f :: ReadCtx m => Table Vectors -> m (Maybe (Vector Int16))
getVectors'g :: ReadCtx m => Table Vectors -> m (Maybe (Vector Int32))
getVectors'h :: ReadCtx m => Table Vectors -> m (Maybe (Vector Int64))
getVectors'i :: ReadCtx m => Table Vectors -> m (Maybe (Vector Float))
getVectors'j :: ReadCtx m => Table Vectors -> m (Maybe (Vector Double))
getVectors'k :: ReadCtx m => Table Vectors -> m (Maybe (Vector Bool))
getVectors'l :: ReadCtx m => Table Vectors -> m (Maybe (Vector Text))
getVectors'a = readTableFieldOpt (readPrimVector Word8Vec)   0
getVectors'b = readTableFieldOpt (readPrimVector Word16Vec)  1
getVectors'c = readTableFieldOpt (readPrimVector Word32Vec)  2
getVectors'd = readTableFieldOpt (readPrimVector Word64Vec)  3
getVectors'e = readTableFieldOpt (readPrimVector Int8Vec)    4
getVectors'f = readTableFieldOpt (readPrimVector Int16Vec)   5
getVectors'g = readTableFieldOpt (readPrimVector Int32Vec)   6
getVectors'h = readTableFieldOpt (readPrimVector Int64Vec)   7
getVectors'i = readTableFieldOpt (readPrimVector FloatVec)   8
getVectors'j = readTableFieldOpt (readPrimVector DoubleVec)  9
getVectors'k = readTableFieldOpt (readPrimVector BoolVec)    10
getVectors'l = readTableFieldOpt (readPrimVector TextVec)    11

----------------------------------
-------- VectorOfTables ----------
----------------------------------
data VectorOfTables

vectorOfTables :: Maybe [WriteTable Axe] -> WriteTable VectorOfTables
vectorOfTables x1 = writeTable
  [ (optional . writeVector) unWriteTable x1
  ]

getVectorOfTables'xs :: ReadCtx m => Table VectorOfTables -> m (Maybe (Vector (Table Axe)))
getVectorOfTables'xs = readTableFieldOpt readTableVector 0

----------------------------------
------- VectorOfStructs ----------
----------------------------------
data VectorOfStructs

vectorOfStructs ::
     Maybe [WriteStruct Struct1]
  -> Maybe [WriteStruct Struct2]
  -> Maybe [WriteStruct Struct3]
  -> Maybe [WriteStruct Struct4]
  -> WriteTable VectorOfStructs
vectorOfStructs x1 x2 x3 x4 = writeTable
  [ (optional . writeVector . inline) unWriteStruct x1
  , (optional . writeVector . inline) unWriteStruct x2
  , (optional . writeVector . inline) unWriteStruct x3
  , (optional . writeVector . inline) unWriteStruct x4
  ]

getVectorOfStructs'as :: ReadCtx m => Table VectorOfStructs -> m (Maybe (Vector (Struct Struct1)))
getVectorOfStructs'as = readTableFieldOpt (readStructVector 3) 0

getVectorOfStructs'bs :: ReadCtx m => Table VectorOfStructs -> m (Maybe (Vector (Struct Struct2)))
getVectorOfStructs'bs = readTableFieldOpt (readStructVector 4) 1

getVectorOfStructs'cs :: ReadCtx m => Table VectorOfStructs -> m (Maybe (Vector (Struct Struct3)))
getVectorOfStructs'cs = readTableFieldOpt (readStructVector 24) 2

getVectorOfStructs'ds :: ReadCtx m => Table VectorOfStructs -> m (Maybe (Vector (Struct Struct4)))
getVectorOfStructs'ds = readTableFieldOpt (readStructVector 24) 3


----------------------------------
------- VectorOfUnions -----------
----------------------------------
data VectorOfUnions

vectorOfUnions :: Maybe [WriteUnion Weapon] -> [WriteUnion Weapon] -> WriteTable VectorOfUnions
vectorOfUnions x1 x2 = writeTable
  [ x1t
  , x1v
  , deprecated
  , deprecated
  , x2t
  , x2v
  ]
  where
    (x1t, x1v) = writeUnionVectorOpt x1
    (x2t, x2v) = writeUnionVectorReq x2

getVectorOfUnions'xs :: ReadCtx m => Table VectorOfUnions -> m (Maybe (Vector (Union Weapon)))
getVectorOfUnions'xs = readTableFieldUnionVectorOpt readWeapon 1

getVectorOfUnions'xsReq :: ReadCtx m => Table VectorOfUnions -> m (Vector (Union Weapon))
getVectorOfUnions'xsReq = readTableFieldUnionVectorReq readWeapon 5 "xsReq"


----------------------------------
---- Scalars with defaults -------
----------------------------------
data ScalarsWithDefaults

scalarsWithDefaults ::
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
  -> Maybe Bool
  -> Maybe Int16
  -> Maybe Int16
  -> WriteTable ScalarsWithDefaults
scalarsWithDefaults a b c d e f g h i j k l m n =
  writeTable
    [ (optionalDef 8 . inline) word8          a
    , (optionalDef 16 . inline) word16        b
    , (optionalDef 32 . inline) word32        c
    , (optionalDef 64 . inline) word64        d
    , (optionalDef (-1) . inline) int8        e
    , (optionalDef (-2) . inline) int16       f
    , (optionalDef (-4) . inline) int32       g
    , (optionalDef (-8) . inline) int64       h
    , (optionalDef 3.9 . inline) float        i
    , (optionalDef (-2.3e10) . inline) double j
    , (optionalDef True . inline) bool        k
    , (optionalDef False . inline) bool       l
    , (optionalDef 1 . inline) int16          m
    , (optionalDef 5 . inline) int16          n
    ]

getScalarsWithDefaults'a :: ReadCtx m => Table ScalarsWithDefaults -> m Word8
getScalarsWithDefaults'b :: ReadCtx m => Table ScalarsWithDefaults -> m Word16
getScalarsWithDefaults'c :: ReadCtx m => Table ScalarsWithDefaults -> m Word32
getScalarsWithDefaults'd :: ReadCtx m => Table ScalarsWithDefaults -> m Word64
getScalarsWithDefaults'e :: ReadCtx m => Table ScalarsWithDefaults -> m Int8
getScalarsWithDefaults'f :: ReadCtx m => Table ScalarsWithDefaults -> m Int16
getScalarsWithDefaults'g :: ReadCtx m => Table ScalarsWithDefaults -> m Int32
getScalarsWithDefaults'h :: ReadCtx m => Table ScalarsWithDefaults -> m Int64
getScalarsWithDefaults'i :: ReadCtx m => Table ScalarsWithDefaults -> m Float
getScalarsWithDefaults'j :: ReadCtx m => Table ScalarsWithDefaults -> m Double
getScalarsWithDefaults'k :: ReadCtx m => Table ScalarsWithDefaults -> m Bool
getScalarsWithDefaults'l :: ReadCtx m => Table ScalarsWithDefaults -> m Bool
getScalarsWithDefaults'm :: ReadCtx m => Table ScalarsWithDefaults -> m Int16
getScalarsWithDefaults'n :: ReadCtx m => Table ScalarsWithDefaults -> m Int16
getScalarsWithDefaults'a = readTableFieldWithDef readWord8   0 8
getScalarsWithDefaults'b = readTableFieldWithDef readWord16  1 16
getScalarsWithDefaults'c = readTableFieldWithDef readWord32  2 32
getScalarsWithDefaults'd = readTableFieldWithDef readWord64  3 64
getScalarsWithDefaults'e = readTableFieldWithDef readInt8    4 (-1)
getScalarsWithDefaults'f = readTableFieldWithDef readInt16   5 (-2)
getScalarsWithDefaults'g = readTableFieldWithDef readInt32   6 (-4)
getScalarsWithDefaults'h = readTableFieldWithDef readInt64   7 (-8)
getScalarsWithDefaults'i = readTableFieldWithDef readFloat   8 3.9
getScalarsWithDefaults'j = readTableFieldWithDef readDouble  9 (-2.3e10)
getScalarsWithDefaults'k = readTableFieldWithDef readBool    10 True
getScalarsWithDefaults'l = readTableFieldWithDef readBool    11 False
getScalarsWithDefaults'm = readTableFieldWithDef readInt16   12 1
getScalarsWithDefaults'n = readTableFieldWithDef readInt16   13 5


----------------------------------
------ Deprecated fields ---------
----------------------------------
data DeprecatedFields

deprecatedFields :: Maybe Int8 -> Maybe Int8 -> Maybe Int8 -> Maybe Int8 -> WriteTable DeprecatedFields
deprecatedFields x0 x1 x2 x3 = writeTable
  [ (optionalDef 0 . inline) int8 x0
  , deprecated
  , (optionalDef 0 . inline) int8 x1
  , deprecated
  , (optionalDef 0 . inline) int8 x2
  , deprecated
  , deprecated
  , (optionalDef 0 . inline) int8 x3
  ]

getDeprecatedFields'a :: ReadCtx m => Table DeprecatedFields -> m Int8
getDeprecatedFields'a = readTableFieldWithDef readInt8 0 0

getDeprecatedFields'c :: ReadCtx m => Table DeprecatedFields -> m Int8
getDeprecatedFields'c = readTableFieldWithDef readInt8 2 0

getDeprecatedFields'e :: ReadCtx m => Table DeprecatedFields -> m Int8
getDeprecatedFields'e = readTableFieldWithDef readInt8 4 0

getDeprecatedFields'g :: ReadCtx m => Table DeprecatedFields -> m Int8
getDeprecatedFields'g = readTableFieldWithDef readInt8 7 0


----------------------------------
-------- Required fields ---------
----------------------------------
data RequiredFields

requiredFields ::
     Text
  -> WriteStruct Struct1
  -> WriteTable Axe
  -> WriteUnion Weapon
  -> [Int32]
  -> WriteTable RequiredFields
requiredFields x0 x1 x2 x3 x4 = writeTable
  [ text x0
  , inline unWriteStruct x1
  , unWriteTable x2
  , writeUnionType x3
  , writeUnionValue x3
  , (writeVector . inline) int32 x4
  ]

getRequiredFields'a :: ReadCtx m => Table RequiredFields -> m Text
getRequiredFields'a = readTableFieldReq readText 0 "a"

getRequiredFields'b :: ReadCtx m => Table RequiredFields -> m (Struct Struct1)
getRequiredFields'b = readTableFieldReq readStruct' 1 "b"

getRequiredFields'c :: ReadCtx m => Table RequiredFields -> m (Table Axe)
getRequiredFields'c = readTableFieldReq readTable 2 "c"

getRequiredFields'd :: ReadCtx m => Table RequiredFields -> m (Union Weapon)
getRequiredFields'd = readTableFieldUnion readWeapon 4

getRequiredFields'e :: ReadCtx m => Table RequiredFields -> m (Vector Int32)
getRequiredFields'e = readTableFieldReq (readPrimVector Int32Vec) 5 "d"

