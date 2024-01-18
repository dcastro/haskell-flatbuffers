{-# LANGUAGE OverloadedStrings #-}

module Examples.HandWritten where

import           Data.Bits                           ((.&.))
import           Data.Int
import           Data.Text                           (Text)
import           Data.Word

import           FlatBuffers.Internal.Build
import           FlatBuffers.Internal.FileIdentifier (HasFileIdentifier (..),
                                                      unsafeFileIdentifier)
import           FlatBuffers.Internal.Read
import           FlatBuffers.Internal.Types
import           FlatBuffers.Internal.Write

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

primitivesA :: Table Primitives -> Either ReadError Word8
primitivesB :: Table Primitives -> Either ReadError Word16
primitivesC :: Table Primitives -> Either ReadError Word32
primitivesD :: Table Primitives -> Either ReadError Word64
primitivesE :: Table Primitives -> Either ReadError Int8
primitivesF :: Table Primitives -> Either ReadError Int16
primitivesG :: Table Primitives -> Either ReadError Int32
primitivesH :: Table Primitives -> Either ReadError Int64
primitivesI :: Table Primitives -> Either ReadError Float
primitivesJ :: Table Primitives -> Either ReadError Double
primitivesK :: Table Primitives -> Either ReadError Bool
primitivesL :: Table Primitives -> Either ReadError (Maybe Text)
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
    0  -> Just ColorGreen
    1  -> Just ColorBlue
    5  -> Just ColorGray
    8  -> Just ColorBlack
    _  -> Nothing

{-# INLINE fromColor #-}
fromColor :: Color -> Int16
fromColor n =
  case n of
    ColorRed   -> -2
    ColorGreen -> 0
    ColorBlue  -> 1
    ColorGray  -> 5
    ColorBlack -> 8

{-# INLINE colorName #-}
colorName :: Color -> Text
colorName c =
  case c of
    ColorRed   -> "Red"
    ColorGreen -> "Green"
    ColorBlue  -> "Blue"
    ColorGray  -> "Gray"
    ColorBlack -> "Black"

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
  , optional writeVectorInt16TableField xs
  , optional writeVectorStructTableField ys
  ]

enumsX :: Table Enums -> Either ReadError Int16
enumsX = readTableFieldWithDef readInt16 0 0

enumsY :: Table Enums -> Either ReadError (Maybe (Struct StructWithEnum))
enumsY = readTableFieldOpt (Right . readStruct) 1

enumsXs :: Table Enums -> Either ReadError (Maybe (Vector Int16))
enumsXs = readTableFieldOpt (readPrimVector VectorInt16) 2

enumsYs :: Table Enums -> Either ReadError (Maybe (Vector (Struct StructWithEnum)))
enumsYs = readTableFieldOpt (readPrimVector VectorStruct) 3



data StructWithEnum

instance IsStruct StructWithEnum where
  structAlignmentOf = 2
  structSizeOf = 6

structWithEnum :: Int8 -> Int16 -> Int8 -> WriteStruct StructWithEnum
structWithEnum x y z = WriteStruct $
  buildInt8 x <> buildPadding 1
  <> buildInt16 y
  <> buildInt8 z <> buildPadding 1

structWithEnumX :: Struct StructWithEnum -> Either ReadError Int8
structWithEnumX = readStructField readInt8 0

structWithEnumY :: Struct StructWithEnum -> Either ReadError Int16
structWithEnumY = readStructField readInt16 2

structWithEnumZ :: Struct StructWithEnum -> Either ReadError Int8
structWithEnumZ = readStructField readInt8 4


----------------------------------
--------- EnumsBitFlags ----------
----------------------------------
colorsRed, colorsGreen, colorsBlue, colorsGray, colorsBlack :: Word16
colorsRed = 1
colorsGreen = 4
colorsBlue = 8
colorsGray = 16
colorsBlack = 32

{-# INLINE allColors #-}
allColors :: [Word16]
allColors = [colorsRed, colorsGreen, colorsBlue, colorsGray, colorsBlack]

{-# INLINE colorsNames #-}
colorsNames :: Word16 -> [Text]
colorsNames c = res5
  where
    res0 = []
    res1 = if colorsBlack .&. c /= 0 then "Black" : res0 else res0
    res2 = if colorsGray  .&. c /= 0 then "Gray"  : res1 else res1
    res3 = if colorsBlue  .&. c /= 0 then "Blue"  : res2 else res2
    res4 = if colorsGreen .&. c /= 0 then "Green" : res3 else res3
    res5 = if colorsRed   .&. c /= 0 then "Red"   : res4 else res4


data EnumsBitFlags

enumsBitFlags ::
    Maybe Word16
  -> Maybe (WriteStruct StructWithEnumBitFlags)
  -> Maybe (WriteVector Word16)
  -> Maybe (WriteVector (WriteStruct StructWithEnumBitFlags))
  -> WriteTable EnumsBitFlags
enumsBitFlags x y xs ys = writeTable
  [ optionalDef 0 writeWord16TableField x
  , optional writeStructTableField y
  , optional writeVectorWord16TableField xs
  , optional writeVectorStructTableField ys
  ]

enumsBitFlagsX :: Table EnumsBitFlags -> Either ReadError Word16
enumsBitFlagsX = readTableFieldWithDef readWord16 0 0

enumsBitFlagsY :: Table EnumsBitFlags -> Either ReadError (Maybe (Struct StructWithEnumBitFlags))
enumsBitFlagsY = readTableFieldOpt (Right . readStruct) 1

enumsBitFlagsXs :: Table EnumsBitFlags -> Either ReadError (Maybe (Vector Word16))
enumsBitFlagsXs = readTableFieldOpt (readPrimVector VectorWord16) 2

enumsBitFlagsYs :: Table EnumsBitFlags -> Either ReadError (Maybe (Vector (Struct StructWithEnumBitFlags)))
enumsBitFlagsYs = readTableFieldOpt (readPrimVector VectorStruct) 3



data StructWithEnumBitFlags

instance IsStruct StructWithEnumBitFlags where
  structAlignmentOf = 2
  structSizeOf = 2

structWithEnumBitFlags :: Word16 -> WriteStruct StructWithEnumBitFlags
structWithEnumBitFlags x = WriteStruct $
  buildWord16 x

structWithEnumBitFlagsX :: Struct StructWithEnumBitFlags -> Either ReadError Word16
structWithEnumBitFlagsX = readStructField readWord16 0

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

struct1X :: Struct Struct1 -> Either ReadError Word8
struct1X = readStructField readWord8 0

struct1Y :: Struct Struct1 -> Either ReadError Int8
struct1Y = readStructField readInt8 1

struct1Z :: Struct Struct1 -> Either ReadError Int8
struct1Z = readStructField readInt8 2


data Struct2
instance IsStruct Struct2 where
  structAlignmentOf = 2
  structSizeOf = 4

struct2 :: Int16 -> WriteStruct Struct2
struct2 x = WriteStruct $
  buildInt16 x <> buildPadding 2

struct2X :: Struct Struct2 -> Either ReadError Int16
struct2X = readStructField readInt16 0


data Struct3
instance IsStruct Struct3 where
  structAlignmentOf = 8
  structSizeOf = 24

struct3 :: WriteStruct Struct2 -> Word64 -> Word8 -> WriteStruct Struct3
struct3 x y z = WriteStruct $
  buildStruct x <> buildPadding 4
  <> buildWord64 y
  <> buildWord8 z <> buildPadding 7

struct3X :: Struct Struct3 -> Struct Struct2
struct3X = readStructField readStruct 0

struct3Y :: Struct Struct3 -> Either ReadError Word64
struct3Y = readStructField readWord64 8

struct3Z :: Struct Struct3 -> Either ReadError Word8
struct3Z = readStructField readWord8 16


data Struct4
instance IsStruct Struct4 where
  structAlignmentOf = 8
  structSizeOf = 24

struct4 :: WriteStruct Struct2 -> Int8 -> Int64 -> Bool -> WriteStruct Struct4
struct4 w x y z = WriteStruct $
  buildStruct w
  <> buildInt8 x <> buildPadding 3
  <> buildInt64 y
  <> buildBool z <> buildPadding 7

struct4W :: Struct Struct4 -> Struct Struct2
struct4W = readStructField readStruct 0

struct4X :: Struct Struct4 -> Either ReadError Int8
struct4X = readStructField readInt8 4

struct4Y :: Struct Struct4 -> Either ReadError Int64
struct4Y = readStructField readInt64 8

struct4Z :: Struct Struct4 -> Either ReadError Bool
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

structsA :: Table Structs -> Either ReadError (Maybe (Struct Struct1))
structsA = readTableFieldOpt (Right . readStruct) 0

structsB :: Table Structs -> Either ReadError (Maybe (Struct Struct2))
structsB = readTableFieldOpt (Right . readStruct) 1

structsC :: Table Structs -> Either ReadError (Maybe (Struct Struct3))
structsC = readTableFieldOpt (Right . readStruct) 2

structsD :: Table Structs -> Either ReadError (Maybe (Struct Struct4))
structsD = readTableFieldOpt (Right . readStruct) 3


----------------------------------
--------- Nested Tables ----------
----------------------------------
data NestedTables

nestedTables :: Maybe (WriteTable Table1) -> WriteTable NestedTables
nestedTables x = writeTable
  [ optional writeTableTableField x
  ]

nestedTablesX :: Table NestedTables -> Either ReadError (Maybe (Table Table1))
nestedTablesX = readTableFieldOpt readTable 0


data Table1

table1 :: Maybe (WriteTable Table2) -> Maybe Int32 -> WriteTable Table1
table1 x y = writeTable
  [ optional writeTableTableField x
  , optionalDef 0 writeInt32TableField y
  ]

table1X :: Table Table1 -> Either ReadError (Maybe (Table Table2))
table1X = readTableFieldOpt readTable 0

table1Y :: Table Table1 -> Either ReadError Int16
table1Y = readTableFieldWithDef readInt16 1 0


data Table2

table2 :: Maybe Int16 -> WriteTable Table2
table2 x = writeTable [ optionalDef 0 writeInt16TableField x ]

table2X :: Table Table2 -> Either ReadError Int16
table2X = readTableFieldWithDef readInt16 0 0

----------------------------------
------------- Sword --------------
----------------------------------
data Sword

sword :: Maybe Text -> WriteTable Sword
sword x = writeTable [optional writeTextTableField x]

swordX :: Table Sword -> Either ReadError (Maybe Text)
swordX = readTableFieldOpt readText 0

----------------------------------
------------- Axe ----------------
----------------------------------
data Axe

axe :: Maybe Int32 -> WriteTable Axe
axe y = writeTable [optionalDef 0 writeInt32TableField y]

axeY :: Table Axe -> Either ReadError Int32
axeY = readTableFieldWithDef readInt32 0 0

----------------------------------
------------- Weapon -------------
----------------------------------
data Weapon
  = WeaponSword !(Table Sword)
  | WeaponAxe !(Table Axe)

weaponSword :: WriteTable Sword -> WriteUnion Weapon
weaponSword = writeUnion 1

weaponAxe :: WriteTable Axe -> WriteUnion Weapon
weaponAxe = writeUnion 2

readWeapon :: Positive Word8 -> PositionInfo -> Either ReadError (Union Weapon)
readWeapon n pos =
  case getPositive n of
    1  -> Union . WeaponSword <$> readTable' pos
    2  -> Union . WeaponAxe <$> readTable' pos
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

tableWithUnionUni :: Table TableWithUnion -> Either ReadError (Union Weapon)
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
    [ optional writeVectorWord8TableField  a
    , optional writeVectorWord16TableField b
    , optional writeVectorWord32TableField c
    , optional writeVectorWord64TableField d
    , optional writeVectorInt8TableField   e
    , optional writeVectorInt16TableField  f
    , optional writeVectorInt32TableField  g
    , optional writeVectorInt64TableField  h
    , optional writeVectorFloatTableField  i
    , optional writeVectorDoubleTableField j
    , optional writeVectorBoolTableField   k
    , optional writeVectorTextTableField   l
    ]

vectorsA :: Table Vectors -> Either ReadError (Maybe (Vector Word8))
vectorsB :: Table Vectors -> Either ReadError (Maybe (Vector Word16))
vectorsC :: Table Vectors -> Either ReadError (Maybe (Vector Word32))
vectorsD :: Table Vectors -> Either ReadError (Maybe (Vector Word64))
vectorsE :: Table Vectors -> Either ReadError (Maybe (Vector Int8))
vectorsF :: Table Vectors -> Either ReadError (Maybe (Vector Int16))
vectorsG :: Table Vectors -> Either ReadError (Maybe (Vector Int32))
vectorsH :: Table Vectors -> Either ReadError (Maybe (Vector Int64))
vectorsI :: Table Vectors -> Either ReadError (Maybe (Vector Float))
vectorsJ :: Table Vectors -> Either ReadError (Maybe (Vector Double))
vectorsK :: Table Vectors -> Either ReadError (Maybe (Vector Bool))
vectorsL :: Table Vectors -> Either ReadError (Maybe (Vector Text))
vectorsA = readTableFieldOpt (readPrimVector VectorWord8)   0
vectorsB = readTableFieldOpt (readPrimVector VectorWord16)  1
vectorsC = readTableFieldOpt (readPrimVector VectorWord32)  2
vectorsD = readTableFieldOpt (readPrimVector VectorWord64)  3
vectorsE = readTableFieldOpt (readPrimVector VectorInt8)    4
vectorsF = readTableFieldOpt (readPrimVector VectorInt16)   5
vectorsG = readTableFieldOpt (readPrimVector VectorInt32)   6
vectorsH = readTableFieldOpt (readPrimVector VectorInt64)   7
vectorsI = readTableFieldOpt (readPrimVector VectorFloat)   8
vectorsJ = readTableFieldOpt (readPrimVector VectorDouble)  9
vectorsK = readTableFieldOpt (readPrimVector VectorBool)    10
vectorsL = readTableFieldOpt (readPrimVector VectorText)    11

----------------------------------
-------- VectorOfTables ----------
----------------------------------
data VectorOfTables

vectorOfTables :: Maybe (WriteVector (WriteTable Axe)) -> WriteTable VectorOfTables
vectorOfTables xs = writeTable
  [ optional writeVectorTableTableField xs
  ]

vectorOfTablesXs :: Table VectorOfTables -> Either ReadError (Maybe (Vector (Table Axe)))
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
  [ optional writeVectorStructTableField as
  , optional writeVectorStructTableField bs
  , optional writeVectorStructTableField cs
  , optional writeVectorStructTableField ds
  ]

vectorOfStructsAs :: Table VectorOfStructs -> Either ReadError (Maybe (Vector (Struct Struct1)))
vectorOfStructsAs = readTableFieldOpt (readPrimVector VectorStruct) 0

vectorOfStructsBs :: Table VectorOfStructs -> Either ReadError (Maybe (Vector (Struct Struct2)))
vectorOfStructsBs = readTableFieldOpt (readPrimVector VectorStruct) 1

vectorOfStructsCs :: Table VectorOfStructs -> Either ReadError (Maybe (Vector (Struct Struct3)))
vectorOfStructsCs = readTableFieldOpt (readPrimVector VectorStruct) 2

vectorOfStructsDs :: Table VectorOfStructs -> Either ReadError (Maybe (Vector (Struct Struct4)))
vectorOfStructsDs = readTableFieldOpt (readPrimVector VectorStruct) 3


----------------------------------
------- VectorOfUnions -----------
----------------------------------
data VectorOfUnions

vectorOfUnions ::
     Maybe (WriteVector (WriteUnion Weapon))
  -> WriteTable VectorOfUnions
vectorOfUnions xs = writeTable
  [ optional writeUnionTypesVectorTableField xs
  , optional writeUnionValuesVectorTableField xs
  ]

vectorOfUnionsXs :: Table VectorOfUnions -> Either ReadError (Maybe (Vector (Union Weapon)))
vectorOfUnionsXs = readTableFieldUnionVectorOpt readWeapon 1

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
  -> Maybe Word16
  -> Maybe Word16
  -> Maybe Word16
  -> Maybe Word16
  -> WriteTable ScalarsWithDefaults
scalarsWithDefaults a b c d e f g h i j k l m n o p q r =
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
    , optionalDef 0 writeWord16TableField         o
    , optionalDef 12 writeWord16TableField        p
    , optionalDef 1 writeWord16TableField         q
    , optionalDef 20 writeWord16TableField        r
    ]

scalarsWithDefaultsA :: Table ScalarsWithDefaults -> Either ReadError Word8
scalarsWithDefaultsB :: Table ScalarsWithDefaults -> Either ReadError Word16
scalarsWithDefaultsC :: Table ScalarsWithDefaults -> Either ReadError Word32
scalarsWithDefaultsD :: Table ScalarsWithDefaults -> Either ReadError Word64
scalarsWithDefaultsE :: Table ScalarsWithDefaults -> Either ReadError Int8
scalarsWithDefaultsF :: Table ScalarsWithDefaults -> Either ReadError Int16
scalarsWithDefaultsG :: Table ScalarsWithDefaults -> Either ReadError Int32
scalarsWithDefaultsH :: Table ScalarsWithDefaults -> Either ReadError Int64
scalarsWithDefaultsI :: Table ScalarsWithDefaults -> Either ReadError Float
scalarsWithDefaultsJ :: Table ScalarsWithDefaults -> Either ReadError Double
scalarsWithDefaultsK :: Table ScalarsWithDefaults -> Either ReadError Bool
scalarsWithDefaultsL :: Table ScalarsWithDefaults -> Either ReadError Bool
scalarsWithDefaultsM :: Table ScalarsWithDefaults -> Either ReadError Int16
scalarsWithDefaultsN :: Table ScalarsWithDefaults -> Either ReadError Int16
scalarsWithDefaultsO :: Table ScalarsWithDefaults -> Either ReadError Word16
scalarsWithDefaultsP :: Table ScalarsWithDefaults -> Either ReadError Word16
scalarsWithDefaultsQ :: Table ScalarsWithDefaults -> Either ReadError Word16
scalarsWithDefaultsR :: Table ScalarsWithDefaults -> Either ReadError Word16
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
scalarsWithDefaultsO = readTableFieldWithDef readWord16  14 0
scalarsWithDefaultsP = readTableFieldWithDef readWord16  15 12
scalarsWithDefaultsQ = readTableFieldWithDef readWord16  16 1
scalarsWithDefaultsR = readTableFieldWithDef readWord16  17 20


----------------------------------
------ Deprecated fields ---------
----------------------------------
data DeprecatedFields

deprecatedFields :: Maybe Int8 -> Maybe Int8 -> Maybe Int8 -> Maybe Int8 -> Maybe Int8 -> WriteTable DeprecatedFields
deprecatedFields a c e g i = writeTable
  [ optionalDef 0 writeInt8TableField a
  , deprecated
  , optionalDef 0 writeInt8TableField c
  , deprecated
  , optionalDef 0 writeInt8TableField e
  , deprecated
  , deprecated
  , optionalDef 0 writeInt8TableField g
  , deprecated
  , deprecated
  , optionalDef 0 writeInt8TableField i
  ]

deprecatedFieldsA :: Table DeprecatedFields -> Either ReadError Int8
deprecatedFieldsA = readTableFieldWithDef readInt8 0 0

deprecatedFieldsC :: Table DeprecatedFields -> Either ReadError Int8
deprecatedFieldsC = readTableFieldWithDef readInt8 2 0

deprecatedFieldsE :: Table DeprecatedFields -> Either ReadError Int8
deprecatedFieldsE = readTableFieldWithDef readInt8 4 0

deprecatedFieldsG :: Table DeprecatedFields -> Either ReadError Int8
deprecatedFieldsG = readTableFieldWithDef readInt8 7 0

deprecatedFieldsI :: Table DeprecatedFields -> Either ReadError Int8
deprecatedFieldsI = readTableFieldWithDef readInt8 10 0

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
  -> WriteVector (WriteUnion Weapon)
  -> WriteTable RequiredFields
requiredFields a b c d e f = writeTable
  [ writeTextTableField a
  , writeStructTableField b
  , writeTableTableField c
  , writeUnionTypeTableField d
  , writeUnionValueTableField d
  , writeVectorInt32TableField e
  , writeUnionTypesVectorTableField f
  , writeUnionValuesVectorTableField f
  ]

requiredFieldsA :: Table RequiredFields -> Either ReadError Text
requiredFieldsA = readTableFieldReq readText 0 "a"

requiredFieldsB :: Table RequiredFields -> Either ReadError (Struct Struct1)
requiredFieldsB = readTableFieldReq (Right . readStruct) 1 "b"

requiredFieldsC :: Table RequiredFields -> Either ReadError (Table Axe)
requiredFieldsC = readTableFieldReq readTable 2 "c"

requiredFieldsD :: Table RequiredFields -> Either ReadError (Union Weapon)
requiredFieldsD = readTableFieldUnion readWeapon 4

requiredFieldsE :: Table RequiredFields -> Either ReadError (Vector Int32)
requiredFieldsE = readTableFieldReq (readPrimVector VectorInt32) 5 "e"

requiredFieldsF :: Table RequiredFields -> Either ReadError (Vector (Union Weapon))
requiredFieldsF = readTableFieldUnionVectorReq readWeapon 7 "f"
