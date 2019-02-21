{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module FlatBuffers.RoundTripSpec where

import           Control.Applicative    (liftA3)
import           Control.Exception.Safe (throwM)
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Int
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import           Data.Word
import           FlatBuffers.Read
import           FlatBuffers.Write
import           Test.Hspec

spec :: Spec
spec =
  describe "Round Trip" $ do
    describe "Primitives" $ do
      it "present" $ do
        x <- decode @Primitives $ encode $ primitives
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just 1234.56) (Just 2873242.82782) (Just True)
        getPrimitives'a x `shouldBe` Just maxBound
        getPrimitives'b x `shouldBe` Just maxBound
        getPrimitives'c x `shouldBe` Just maxBound
        getPrimitives'd x `shouldBe` Just maxBound
        getPrimitives'e x `shouldBe` Just maxBound
        getPrimitives'f x `shouldBe` Just maxBound
        getPrimitives'g x `shouldBe` Just maxBound
        getPrimitives'h x `shouldBe` Just maxBound
        getPrimitives'i x `shouldBe` Just 1234.56
        getPrimitives'j x `shouldBe` Just 2873242.82782
        getPrimitives'k x `shouldBe` Just True
      it "missing" $ do
        x <- decode @Primitives $ encode $ primitives
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing Nothing
          Nothing Nothing Nothing
        getPrimitives'a x `shouldBe` Just 1
        getPrimitives'b x `shouldBe` Just 1
        getPrimitives'c x `shouldBe` Just 1
        getPrimitives'd x `shouldBe` Just 1
        getPrimitives'e x `shouldBe` Just 1
        getPrimitives'f x `shouldBe` Just 1
        getPrimitives'g x `shouldBe` Just 1
        getPrimitives'h x `shouldBe` Just 1
        getPrimitives'i x `shouldBe` Just 1
        getPrimitives'j x `shouldBe` Just 1
        getPrimitives'k x `shouldBe` Just False

    describe "Enums" $ do
      it "present" $ do
        x <- decode $ encode $ enums (Just Gray)
        getEnums'x x `shouldBe` Just Gray
      it "missing" $ do
        x <- decode @Enums $ encode $ enums Nothing
        getEnums'x x `shouldBe` Just Blue

    describe "Union" $ do
      it "present" $ do
        x <- decode $ encode $ tableWithUnion (Just (union'unionA (unionA (Just "hi"))))
        getTableWithUnion'uni x >>= \case
          Union'UnionA x -> getUnionA'x req x `shouldBe` Just "hi"
          _              -> unexpectedUnionType

        x <- decode $ encode $ tableWithUnion (Just (union'unionB (unionB (Just maxBound))))
        getTableWithUnion'uni x >>= \case
          Union'UnionB x -> getUnionB'y x `shouldBe` Just maxBound
          _              -> unexpectedUnionType

        x <- decode $ encode $ tableWithUnion (Just none)
        getTableWithUnion'uni x >>= \case
          Union'None -> pure ()
          _          -> unexpectedUnionType

      it "missing" $ do
        x <- decode $ encode $ tableWithUnion Nothing
        getTableWithUnion'uni x >>= \case
          Union'None -> pure ()
          _          -> unexpectedUnionType

    describe "VectorOfUnions" $ do
      it "present" $ do
        x <- decode $ encode $ vectorOfUnions (Just
          [ union'unionA (unionA (Just "hi"))
          , none
          , union'unionB (unionB (Just 98))
          ])
        xs <- getVectorOfUnions'xs req x
        vectorLength xs `shouldBe` 3
        xs `index` 0 >>= \case
          Union'UnionA x -> getUnionA'x req x `shouldBe` Just "hi"
          _              -> unexpectedUnionType
        xs `index` 1 >>= \case
          Union'None -> pure ()
          _          -> unexpectedUnionType
        xs `index` 2 >>= \case
          Union'UnionB x -> getUnionB'y x `shouldBe` Just 98
          _              -> unexpectedUnionType
        xs `index` 3 `shouldThrow` \err -> err == VectorIndexOutOfBounds 3 3

      it "missing" $ do
        x <- decode $ encode $ vectorOfUnions Nothing
        getVectorOfUnions'xs req x `shouldThrow` \err -> err == MissingField "xs"
        getVectorOfUnions'xs opt x >>= \mb -> isNothing mb `shouldBe` True

    describe "VectorOfStructs" $ do
      let getBytes = (liftA3 . liftA3) (,,) getThreeBytes'a getThreeBytes'b getThreeBytes'c
      it "present" $ do
        x <- decode @VectorOfStructs $ encode $ vectorOfStructs (Just
          [ threeBytes 1 2 3
          , threeBytes 4 5 6
          ])
        xs <- getVectorOfStructs'xs req x
        (toList xs >>= traverse getBytes) `shouldBe` Just [(1,2,3), (4,5,6)]
      it "missing" $ do
        x <- decode @VectorOfStructs $ encode $ vectorOfStructs Nothing
        getVectorOfStructs'xs req x `shouldThrow` \err -> err == MissingField "xs"
        getVectorOfStructs'xs opt x >>= \mb -> isNothing mb `shouldBe` True

unexpectedUnionType = expectationFailure "Unexpected union type"

----------------------------------
---------- Primitives ------------
----------------------------------
newtype Primitives =
  Primitives Table

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
  = Union'None
  | Union'UnionA !UnionA
  | Union'UnionB !UnionB

union'unionA :: WriteTable UnionA -> WriteUnion Union
union'unionA = writeUnion 1

union'unionB :: WriteTable UnionB -> WriteUnion Union
union'unionB = writeUnion 2

readUnion :: ReadCtx m => Word8 -> Position -> m Union
readUnion n pos =
  case n of
    0 -> pure Union'None
    1 -> fmap Union'UnionA (readTable pos)
    2 -> fmap Union'UnionB (readTable pos)
    _ -> throwM $ UnionUnknown "Union" n

----------------------------------
------- TableWithUnion -----------
----------------------------------
newtype TableWithUnion =
  TableWithUnion Table

tableWithUnion :: Maybe (WriteUnion Union) -> WriteTable TableWithUnion
tableWithUnion x1 =
  writeTable [wType x1, wValue x1]


getTableWithUnion'uni :: ReadCtx m => TableWithUnion -> m Union
getTableWithUnion'uni = readTableFieldUnion readUnion 0 Union'None

----------------------------------
------- VectorOfUnions -----------
----------------------------------
newtype VectorOfUnions =
  VectorOfUnions Table

vectorOfUnions :: Maybe [WriteUnion Union] -> WriteTable VectorOfUnions
vectorOfUnions x1 =
  writeTable [wType x1, wValue x1]

getVectorOfUnions'xs :: ReadCtx m => ReadMode (Vector Union) a -> VectorOfUnions -> m a
getVectorOfUnions'xs = readTableFieldUnionVector readUnion 0 "xs" Union'None


----------------------------------
----------- ThreeBytes -----------
----------------------------------
newtype ThreeBytes = ThreeBytes Struct

threeBytes :: Word8 -> Word8 -> Word8 -> WriteStruct ThreeBytes
threeBytes a b c =
  writeStruct
    ( ws a )
    [ ws b
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
