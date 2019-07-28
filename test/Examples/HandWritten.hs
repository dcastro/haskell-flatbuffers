{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.HandWritten where

import           Data.Coerce                   ( coerce )
import           Data.Int
import           Data.Text                     ( Text )
import           Data.Word

import           FlatBuffers.FileIdentifier    ( HasFileIdentifier(..), unsafeFileIdentifier )
import           FlatBuffers.Internal.Build
import           FlatBuffers.Internal.Positive ( Positive(getPositive) )
import           FlatBuffers.Internal.Write
import           FlatBuffers.Read
import           FlatBuffers.Types

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
    [ optionalDef 0 writeWord8TableField    a
    , optionalDef 0 writeWord16TableField   b
    , optionalDef 0 writeWord32TableField   c
    , optionalDef 0 writeWord64TableField   d
    , optionalDef 0 writeInt8TableField     e
    , optionalDef 0 writeInt16TableField    f
    , optionalDef 0 writeInt32TableField    g
    , optionalDef 0 writeInt64TableField    h
    , optionalDef 0 writeFloatTableField    i
    , optionalDef 0 writeDoubleTableField   j
    , optionalDef False writeBoolTableField k
    , optional writeTextTableField          l
    ]

primitivesA :: ReadCtx m => Table Primitives -> m Word8
primitivesB :: ReadCtx m => Table Primitives -> m Word16
primitivesC :: ReadCtx m => Table Primitives -> m Word32
primitivesD :: ReadCtx m => Table Primitives -> m Word64
primitivesE :: ReadCtx m => Table Primitives -> m Int8
primitivesF :: ReadCtx m => Table Primitives -> m Int16
primitivesG :: ReadCtx m => Table Primitives -> m Int32
primitivesH :: ReadCtx m => Table Primitives -> m Int64
primitivesI :: ReadCtx m => Table Primitives -> m Float
primitivesJ :: ReadCtx m => Table Primitives -> m Double
primitivesK :: ReadCtx m => Table Primitives -> m Bool
primitivesL :: ReadCtx m => Table Primitives -> m (Maybe Text)
primitivesA = readTableFieldWithDef readWord8   0 0
primitivesB = readTableFieldWithDef readWord16  1 0
primitivesC = readTableFieldWithDef readWord32  2 0
primitivesD = readTableFieldWithDef readWord64  3 0
primitivesE = readTableFieldWithDef readInt8    4 0
primitivesF = readTableFieldWithDef readInt16   5 0
primitivesG = readTableFieldWithDef readInt32   6 0
primitivesH = readTableFieldWithDef readInt64   7 0
primitivesI = readTableFieldWithDef readFloat   8 0
primitivesJ = readTableFieldWithDef readDouble  9 0
primitivesK = readTableFieldWithDef readBool    10 False
primitivesL = readTableFieldOpt     readText    11

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

enums ::
    Maybe Int16
  -> Maybe (WriteStruct StructWithEnum)
  -> Maybe (WriteVector Int16)
  -> Maybe (WriteVector (WriteStruct StructWithEnum))
  -> WriteTable Enums
enums x y xs ys = writeTable
  [ optionalDef 0 writeInt16TableField x
  , optional writeStructTableField y
  , optional writeVectorTableField xs
  , optional writeVectorTableField ys
  ]

enumsX :: ReadCtx m => Table Enums -> m Int16
enumsX = readTableFieldWithDef readInt16 0 0

enumsY :: ReadCtx m => Table Enums -> m (Maybe (Struct StructWithEnum))
enumsY = readTableFieldOpt readStruct' 1

enumsXs :: ReadCtx m => Table Enums -> m (Maybe (Vector Int16))
enumsXs = readTableFieldOpt (readPrimVector Int16Vec) 2

enumsYs :: ReadCtx m => Table Enums -> m (Maybe (Vector (Struct StructWithEnum)))
enumsYs = readTableFieldOpt readStructVector 3



data StructWithEnum

instance IsStruct StructWithEnum where
  structAlignmentOf = 2
  structSizeOf = 6

structWithEnum :: Int8 -> Int16 -> Int8 -> WriteStruct StructWithEnum
structWithEnum x y z = WriteStruct $
  buildInt8 x <> buildPadding 1
  <> buildInt16 y
  <> buildInt8 z <> buildPadding 1

structWithEnumX :: ReadCtx m => Struct StructWithEnum -> m Int8
structWithEnumX = readStructField readInt8 0

structWithEnumY :: ReadCtx m => Struct StructWithEnum -> m Int16
structWithEnumY = readStructField readInt16 2

structWithEnumZ :: ReadCtx m => Struct StructWithEnum -> m Int8
structWithEnumZ = readStructField readInt8 4

----------------------------------
------------- Structs ------------
----------------------------------
data Struct1
instance IsStruct Struct1 where
  structAlignmentOf = 1
  structSizeOf = 3

struct1 :: Word8 -> Int8 -> Int8 -> WriteStruct Struct1
struct1 x y z =
  WriteStruct $
    buildWord8 x <> buildInt8 y <> buildInt8 z

struct1X :: ReadCtx m => Struct Struct1 -> m Word8
struct1X = readStructField readWord8 0

struct1Y :: ReadCtx m => Struct Struct1 -> m Int8
struct1Y = readStructField readInt8 1

struct1Z :: ReadCtx m => Struct Struct1 -> m Int8
struct1Z = readStructField readInt8 2


data Struct2
instance IsStruct Struct2 where
  structAlignmentOf = 2
  structSizeOf = 4

struct2 :: Int16 -> WriteStruct Struct2
struct2 x = WriteStruct $
  buildInt16 x <> buildPadding 2

struct2X :: ReadCtx m => Struct Struct2 -> m Int16
struct2X = readStructField readInt16 0


data Struct3
instance IsStruct Struct3 where
  structAlignmentOf = 8
  structSizeOf = 24

struct3 :: WriteStruct Struct2 -> Word64 -> Word8 -> WriteStruct Struct3
struct3 x y z = WriteStruct $
  coerce x <> buildPadding 4
  <> buildWord64 y
  <> buildWord8 z <> buildPadding 7

struct3X :: Struct Struct3 -> Struct Struct2
struct3X = readStructField readStruct 0

struct3Y :: ReadCtx m => Struct Struct3 -> m Word64
struct3Y = readStructField readWord64 8

struct3Z :: ReadCtx m => Struct Struct3 -> m Word8
struct3Z = readStructField readWord8 16


data Struct4
instance IsStruct Struct4 where
  structAlignmentOf = 8
  structSizeOf = 24

struct4 :: WriteStruct Struct2 -> Int8 -> Int64 -> Bool -> WriteStruct Struct4
struct4 w x y z = WriteStruct $
  coerce w
  <> buildInt8 x <> buildPadding 3
  <> buildInt64 y
  <> buildBool z <> buildPadding 7

struct4W :: Struct Struct4 -> Struct Struct2
struct4W = readStructField readStruct 0

struct4X :: ReadCtx m => Struct Struct4 -> m Int8
struct4X = readStructField readInt8 4

struct4Y :: ReadCtx m => Struct Struct4 -> m Int64
struct4Y = readStructField readInt64 8

struct4Z :: ReadCtx m => Struct Struct4 -> m Bool
struct4Z = readStructField readBool 16


data Structs

structs ::
     Maybe (WriteStruct Struct1)
  -> Maybe (WriteStruct Struct2)
  -> Maybe (WriteStruct Struct3)
  -> Maybe (WriteStruct Struct4)
  -> WriteTable Structs
structs a b c d = writeTable
  [ optional writeStructTableField a
  , optional writeStructTableField b
  , optional writeStructTableField c
  , optional writeStructTableField d
  ]

structsA :: ReadCtx m => Table Structs -> m (Maybe (Struct Struct1))
structsA = readTableFieldOpt readStruct' 0

structsB :: ReadCtx m => Table Structs -> m (Maybe (Struct Struct2))
structsB = readTableFieldOpt readStruct' 1

structsC :: ReadCtx m => Table Structs -> m (Maybe (Struct Struct3))
structsC = readTableFieldOpt readStruct' 2

structsD :: ReadCtx m => Table Structs -> m (Maybe (Struct Struct4))
structsD = readTableFieldOpt readStruct' 3


----------------------------------
--------- Nested Tables ----------
----------------------------------
data NestedTables

nestedTables :: Maybe (WriteTable Table1) -> WriteTable NestedTables
nestedTables x = writeTable
  [ optional writeTableTableField x
  ]

nestedTablesX :: ReadCtx m => Table NestedTables -> m (Maybe (Table Table1))
nestedTablesX = readTableFieldOpt readTable 0


data Table1

table1 :: Maybe (WriteTable Table2) -> Maybe Int32 -> WriteTable Table1
table1 x y = writeTable
  [ optional writeTableTableField x
  , optionalDef 0 writeInt32TableField y
  ]

table1X :: ReadCtx m => Table Table1 -> m (Maybe (Table Table2))
table1X = readTableFieldOpt readTable 0

table1Y :: ReadCtx m => Table Table1 -> m Int16
table1Y = readTableFieldWithDef readInt16 1 0


data Table2

table2 :: Maybe Int16 -> WriteTable Table2
table2 x = writeTable [ optionalDef 0 writeInt16TableField x ]

table2X :: ReadCtx m => Table Table2 -> m Int16
table2X = readTableFieldWithDef readInt16 0 0

----------------------------------
------------- Sword --------------
----------------------------------
data Sword

sword :: Maybe Text -> WriteTable Sword
sword x = writeTable [optional writeTextTableField x]

swordX :: ReadCtx m => Table Sword -> m (Maybe Text)
swordX = readTableFieldOpt readText 0

----------------------------------
------------- Axe ----------------
----------------------------------
data Axe

axe :: Maybe Int32 -> WriteTable Axe
axe y = writeTable [optionalDef 0 writeInt32TableField y]

axeY :: ReadCtx m => Table Axe -> m Int32
axeY = readTableFieldWithDef readInt32 0 0

----------------------------------
------------- Weapon --------------
----------------------------------
data Weapon
  = WeaponSword !(Table Sword)
  | WeaponAxe !(Table Axe)

weaponSword :: WriteTable Sword -> WriteUnion Weapon
weaponSword = writeUnion 1

weaponAxe :: WriteTable Axe -> WriteUnion Weapon
weaponAxe = writeUnion 2

readWeapon :: ReadCtx m => Positive Word8 -> PositionInfo -> m (Union Weapon)
readWeapon n pos =
  case getPositive n of
    1  -> Union . WeaponSword <$> readTable pos
    2  -> Union . WeaponAxe <$> readTable pos
    n' -> pure $! UnionUnknown n'

----------------------------------
------- TableWithUnion -----------
----------------------------------
data TableWithUnion

tableWithUnion :: WriteUnion Weapon -> WriteTable TableWithUnion
tableWithUnion uni = writeTable
  [ writeUnionTypeTableField uni
  , writeUnionValueTableField uni
  ]

tableWithUnionUni :: ReadCtx m => Table TableWithUnion -> m (Union Weapon)
tableWithUnionUni = readTableFieldUnion readWeapon 1

----------------------------------
------------ Vectors -------------
----------------------------------
data Vectors

vectors ::
     Maybe (WriteVector Word8)
  -> Maybe (WriteVector Word16)
  -> Maybe (WriteVector Word32)
  -> Maybe (WriteVector Word64)
  -> Maybe (WriteVector Int8)
  -> Maybe (WriteVector Int16)
  -> Maybe (WriteVector Int32)
  -> Maybe (WriteVector Int64)
  -> Maybe (WriteVector Float)
  -> Maybe (WriteVector Double)
  -> Maybe (WriteVector Bool)
  -> Maybe (WriteVector Text)
  -> WriteTable Vectors
vectors a b c d e f g h i j k l =
  writeTable
    [ optional writeVectorTableField a
    , optional writeVectorTableField b
    , optional writeVectorTableField c
    , optional writeVectorTableField d
    , optional writeVectorTableField e
    , optional writeVectorTableField f
    , optional writeVectorTableField g
    , optional writeVectorTableField h
    , optional writeVectorTableField i
    , optional writeVectorTableField j
    , optional writeVectorTableField k
    , optional writeVectorTableField l
    ]

vectorsA :: ReadCtx m => Table Vectors -> m (Maybe (Vector Word8))
vectorsB :: ReadCtx m => Table Vectors -> m (Maybe (Vector Word16))
vectorsC :: ReadCtx m => Table Vectors -> m (Maybe (Vector Word32))
vectorsD :: ReadCtx m => Table Vectors -> m (Maybe (Vector Word64))
vectorsE :: ReadCtx m => Table Vectors -> m (Maybe (Vector Int8))
vectorsF :: ReadCtx m => Table Vectors -> m (Maybe (Vector Int16))
vectorsG :: ReadCtx m => Table Vectors -> m (Maybe (Vector Int32))
vectorsH :: ReadCtx m => Table Vectors -> m (Maybe (Vector Int64))
vectorsI :: ReadCtx m => Table Vectors -> m (Maybe (Vector Float))
vectorsJ :: ReadCtx m => Table Vectors -> m (Maybe (Vector Double))
vectorsK :: ReadCtx m => Table Vectors -> m (Maybe (Vector Bool))
vectorsL :: ReadCtx m => Table Vectors -> m (Maybe (Vector Text))
vectorsA = readTableFieldOpt (readPrimVector Word8Vec)   0
vectorsB = readTableFieldOpt (readPrimVector Word16Vec)  1
vectorsC = readTableFieldOpt (readPrimVector Word32Vec)  2
vectorsD = readTableFieldOpt (readPrimVector Word64Vec)  3
vectorsE = readTableFieldOpt (readPrimVector Int8Vec)    4
vectorsF = readTableFieldOpt (readPrimVector Int16Vec)   5
vectorsG = readTableFieldOpt (readPrimVector Int32Vec)   6
vectorsH = readTableFieldOpt (readPrimVector Int64Vec)   7
vectorsI = readTableFieldOpt (readPrimVector FloatVec)   8
vectorsJ = readTableFieldOpt (readPrimVector DoubleVec)  9
vectorsK = readTableFieldOpt (readPrimVector BoolVec)    10
vectorsL = readTableFieldOpt (readPrimVector TextVec)    11

----------------------------------
-------- VectorOfTables ----------
----------------------------------
data VectorOfTables

vectorOfTables :: Maybe (WriteVector (WriteTable Axe)) -> WriteTable VectorOfTables
vectorOfTables xs = writeTable
  [ optional writeVectorTableField xs
  ]

vectorOfTablesXs :: ReadCtx m => Table VectorOfTables -> m (Maybe (Vector (Table Axe)))
vectorOfTablesXs = readTableFieldOpt readTableVector 0

----------------------------------
------- VectorOfStructs ----------
----------------------------------
data VectorOfStructs

vectorOfStructs ::
     Maybe (WriteVector (WriteStruct Struct1))
  -> Maybe (WriteVector (WriteStruct Struct2))
  -> Maybe (WriteVector (WriteStruct Struct3))
  -> Maybe (WriteVector (WriteStruct Struct4))
  -> WriteTable VectorOfStructs
vectorOfStructs as bs cs ds = writeTable
  [ optional writeVectorTableField as
  , optional writeVectorTableField bs
  , optional writeVectorTableField cs
  , optional writeVectorTableField ds
  ]

vectorOfStructsAs :: ReadCtx m => Table VectorOfStructs -> m (Maybe (Vector (Struct Struct1)))
vectorOfStructsAs = readTableFieldOpt readStructVector 0

vectorOfStructsBs :: ReadCtx m => Table VectorOfStructs -> m (Maybe (Vector (Struct Struct2)))
vectorOfStructsBs = readTableFieldOpt readStructVector 1

vectorOfStructsCs :: ReadCtx m => Table VectorOfStructs -> m (Maybe (Vector (Struct Struct3)))
vectorOfStructsCs = readTableFieldOpt readStructVector 2

vectorOfStructsDs :: ReadCtx m => Table VectorOfStructs -> m (Maybe (Vector (Struct Struct4)))
vectorOfStructsDs = readTableFieldOpt readStructVector 3


----------------------------------
------- VectorOfUnions -----------
----------------------------------
data VectorOfUnions

vectorOfUnions ::
     Maybe (WriteVector (WriteUnion Weapon))
  -> WriteVector (WriteUnion Weapon)
  -> WriteTable VectorOfUnions
vectorOfUnions xs xsReq = writeTable
  [ optional writeUnionTypesVectorTableField xs
  , optional writeUnionValuesVectorTableField xs
  , deprecated
  , deprecated
  , writeUnionTypesVectorTableField xsReq
  , writeUnionValuesVectorTableField xsReq
  ]

vectorOfUnionsXs :: ReadCtx m => Table VectorOfUnions -> m (Maybe (Vector (Union Weapon)))
vectorOfUnionsXs = readTableFieldUnionVectorOpt readWeapon 1

vectorOfUnionsXsReq :: ReadCtx m => Table VectorOfUnions -> m (Vector (Union Weapon))
vectorOfUnionsXsReq = readTableFieldUnionVectorReq readWeapon 5 "xsReq"


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
    [ optionalDef 8 writeWord8TableField          a
    , optionalDef 16 writeWord16TableField        b
    , optionalDef 32 writeWord32TableField        c
    , optionalDef 64 writeWord64TableField        d
    , optionalDef (-1) writeInt8TableField        e
    , optionalDef (-2) writeInt16TableField       f
    , optionalDef (-4) writeInt32TableField       g
    , optionalDef (-8) writeInt64TableField       h
    , optionalDef 3.9 writeFloatTableField        i
    , optionalDef (-2.3e10) writeDoubleTableField j
    , optionalDef True writeBoolTableField        k
    , optionalDef False writeBoolTableField       l
    , optionalDef 1 writeInt16TableField          m
    , optionalDef 5 writeInt16TableField          n
    ]

scalarsWithDefaultsA :: ReadCtx m => Table ScalarsWithDefaults -> m Word8
scalarsWithDefaultsB :: ReadCtx m => Table ScalarsWithDefaults -> m Word16
scalarsWithDefaultsC :: ReadCtx m => Table ScalarsWithDefaults -> m Word32
scalarsWithDefaultsD :: ReadCtx m => Table ScalarsWithDefaults -> m Word64
scalarsWithDefaultsE :: ReadCtx m => Table ScalarsWithDefaults -> m Int8
scalarsWithDefaultsF :: ReadCtx m => Table ScalarsWithDefaults -> m Int16
scalarsWithDefaultsG :: ReadCtx m => Table ScalarsWithDefaults -> m Int32
scalarsWithDefaultsH :: ReadCtx m => Table ScalarsWithDefaults -> m Int64
scalarsWithDefaultsI :: ReadCtx m => Table ScalarsWithDefaults -> m Float
scalarsWithDefaultsJ :: ReadCtx m => Table ScalarsWithDefaults -> m Double
scalarsWithDefaultsK :: ReadCtx m => Table ScalarsWithDefaults -> m Bool
scalarsWithDefaultsL :: ReadCtx m => Table ScalarsWithDefaults -> m Bool
scalarsWithDefaultsM :: ReadCtx m => Table ScalarsWithDefaults -> m Int16
scalarsWithDefaultsN :: ReadCtx m => Table ScalarsWithDefaults -> m Int16
scalarsWithDefaultsA = readTableFieldWithDef readWord8   0 8
scalarsWithDefaultsB = readTableFieldWithDef readWord16  1 16
scalarsWithDefaultsC = readTableFieldWithDef readWord32  2 32
scalarsWithDefaultsD = readTableFieldWithDef readWord64  3 64
scalarsWithDefaultsE = readTableFieldWithDef readInt8    4 (-1)
scalarsWithDefaultsF = readTableFieldWithDef readInt16   5 (-2)
scalarsWithDefaultsG = readTableFieldWithDef readInt32   6 (-4)
scalarsWithDefaultsH = readTableFieldWithDef readInt64   7 (-8)
scalarsWithDefaultsI = readTableFieldWithDef readFloat   8 3.9
scalarsWithDefaultsJ = readTableFieldWithDef readDouble  9 (-2.3e10)
scalarsWithDefaultsK = readTableFieldWithDef readBool    10 True
scalarsWithDefaultsL = readTableFieldWithDef readBool    11 False
scalarsWithDefaultsM = readTableFieldWithDef readInt16   12 1
scalarsWithDefaultsN = readTableFieldWithDef readInt16   13 5


----------------------------------
------ Deprecated fields ---------
----------------------------------
data DeprecatedFields

deprecatedFields :: Maybe Int8 -> Maybe Int8 -> Maybe Int8 -> Maybe Int8 -> WriteTable DeprecatedFields
deprecatedFields a c e g = writeTable
  [ optionalDef 0 writeInt8TableField a
  , deprecated
  , optionalDef 0 writeInt8TableField c
  , deprecated
  , optionalDef 0 writeInt8TableField e
  , deprecated
  , deprecated
  , optionalDef 0 writeInt8TableField g
  ]

deprecatedFieldsA :: ReadCtx m => Table DeprecatedFields -> m Int8
deprecatedFieldsA = readTableFieldWithDef readInt8 0 0

deprecatedFieldsC :: ReadCtx m => Table DeprecatedFields -> m Int8
deprecatedFieldsC = readTableFieldWithDef readInt8 2 0

deprecatedFieldsE :: ReadCtx m => Table DeprecatedFields -> m Int8
deprecatedFieldsE = readTableFieldWithDef readInt8 4 0

deprecatedFieldsG :: ReadCtx m => Table DeprecatedFields -> m Int8
deprecatedFieldsG = readTableFieldWithDef readInt8 7 0


----------------------------------
-------- Required fields ---------
----------------------------------
data RequiredFields

requiredFields ::
     Text
  -> WriteStruct Struct1
  -> WriteTable Axe
  -> WriteUnion Weapon
  -> WriteVector Int32
  -> WriteTable RequiredFields
requiredFields a b c d e = writeTable
  [ writeTextTableField a
  , writeStructTableField b
  , writeTableTableField c
  , writeUnionTypeTableField d
  , writeUnionValueTableField d
  , writeVectorTableField e
  ]

requiredFieldsA :: ReadCtx m => Table RequiredFields -> m Text
requiredFieldsA = readTableFieldReq readText 0 "a"

requiredFieldsB :: ReadCtx m => Table RequiredFields -> m (Struct Struct1)
requiredFieldsB = readTableFieldReq readStruct' 1 "b"

requiredFieldsC :: ReadCtx m => Table RequiredFields -> m (Table Axe)
requiredFieldsC = readTableFieldReq readTable 2 "c"

requiredFieldsD :: ReadCtx m => Table RequiredFields -> m (Union Weapon)
requiredFieldsD = readTableFieldUnion readWeapon 4

requiredFieldsE :: ReadCtx m => Table RequiredFields -> m (Vector Int32)
requiredFieldsE = readTableFieldReq (readPrimVector Int32Vec) 5 "d"

