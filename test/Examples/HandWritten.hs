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
  = ColorRed
  | ColorGreen
  | ColorBlue
  | ColorGray
  | ColorBlack
  deriving (Eq, Show, Read, Ord, Bounded)

instance AsTableField Color where
  w = inline ws
instance AsStructField Color where
  ws = ws . fromColor

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
newtype Enums =
  Enums Table

enums :: Maybe Word16 -> Maybe (WriteStruct StructWithEnum) -> [Word16] -> Maybe [WriteStruct StructWithEnum] -> WriteTable Enums
enums x1 x2 x3 x4 = writeTable [w x1, w x2, w x3, w x4]

getEnums'x :: ReadCtx m => Enums -> m Word16
getEnums'x = readTableFieldWithDef readWord16 0 2

getEnums'y :: ReadCtx m => ReadMode StructWithEnum a -> Enums -> m a
getEnums'y = readTableField readStruct' 1 "y"

getEnums'xs :: ReadCtx m => Enums -> m (Vector Word16)
getEnums'xs = readTableField (readVector readWord16 2) 2 "xs" req

getEnums'ys :: ReadCtx m => ReadMode (Vector StructWithEnum) a -> Enums -> m a
getEnums'ys = readTableField (readVector readStruct' 6) 3 "ys"



newtype StructWithEnum = StructWithEnum Struct

structWithEnum :: Int8 -> Word16 -> Int8 -> WriteStruct StructWithEnum
structWithEnum x1 x2 x3 = writeStruct 2 [W.padded 1 $ ws x1, ws x2, W.padded 1 $ ws x3]

getStructWithEnum'x :: ReadCtx m => StructWithEnum -> m Int8
getStructWithEnum'x = readStructField readInt8 0

getStructWithEnum'y :: ReadCtx m => StructWithEnum -> m Word16
getStructWithEnum'y = readStructField readWord16 2

getStructWithEnum'z :: ReadCtx m => StructWithEnum -> m Int8
getStructWithEnum'z = readStructField readInt8 4


----------------------------------
------------- Sword -------------
----------------------------------
newtype Sword =
  Sword Table

sword :: Maybe Text -> WriteTable Sword
sword x1 = writeTable [w x1]

getSword'x :: ReadCtx m => ReadMode Text a -> Sword -> m a
getSword'x = readTableField readText 0 "x"

----------------------------------
------------- Axe -------------
----------------------------------
newtype Axe =
  Axe Table

axe :: Maybe Int32 -> WriteTable Axe
axe x1 = writeTable [w x1]

getAxe'y :: ReadCtx m => Axe -> m Int32
getAxe'y = readTableFieldWithDef readInt32 0 0

----------------------------------
------------- Weapon --------------
----------------------------------
data Weapon
  = Weapon'Sword !Sword
  | Weapon'Axe !Axe

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
newtype TableWithUnion =
  TableWithUnion Table

tableWithUnion :: Maybe (WriteUnion Weapon) -> WriteUnion Weapon -> WriteTable TableWithUnion
tableWithUnion x1 x2 =
  writeTable [wType x1, wValue x1, wType x2, wValue x2]

getTableWithUnion'uni :: ReadCtx m => TableWithUnion -> m (Union Weapon)
getTableWithUnion'uni = readTableFieldUnion readWeapon 0

getTableWithUnion'uniReq :: ReadCtx m => TableWithUnion -> m (Union Weapon)
getTableWithUnion'uniReq = readTableFieldUnion readWeapon 1

----------------------------------
------- VectorOfUnions -----------
----------------------------------
newtype VectorOfUnions =
  VectorOfUnions Table

vectorOfUnions :: Maybe [WriteUnion Weapon] -> WriteTable VectorOfUnions
vectorOfUnions x1 =
  writeTable [wType x1, wValue x1]

getVectorOfUnions'xs :: ReadCtx m => ReadMode (Vector (Union Weapon)) a -> VectorOfUnions -> m a
getVectorOfUnions'xs = readTableFieldUnionVector readWeapon 0 "xs"

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

